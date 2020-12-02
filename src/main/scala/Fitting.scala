// breeze
import breeze.linalg.{DenseMatrix, DenseVector}

// scalismo
import scalismo.common.PointId
import scalismo.geometry.{Landmark, Point, Point3D, _3D}
import scalismo.mesh.{TriangleMesh, TriangleMesh3D}
import scalismo.numerics.UniformMeshSampler3D
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.{MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.mesh.MeshMetrics

import vtk.vtkPoints

package Helper {

  class Fitting {

    // number of iterations for fitting
    val fittingIterations: Int = 15

    // number of samples on target mesh (to create posterior model)
    val samplesOnTargetMesh: Int = 50

      def fit(model: StatisticalMeshModel, targetMesh: TriangleMesh[_3D], targetLandmarks: Seq[Landmark[_3D]], landmarks: vtkPoints, ids: Array[Int], labels: Array[String]): Double = {

        // get model landmarks by landmarks, labels, ids
        val modelLandmarks = vtkPointsToSeqLandmark3D(landmarks, labels)
        val modelLandmarksIDs = ids.map(i => PointId(i))

        // rigidly aligning target with model
        val rigidTransform = LandmarkRegistration.rigid3DLandmarkRegistration(targetLandmarks, modelLandmarks, Point(0, 0, 0))

        val alignedTargetMesh = targetMesh.transform(rigidTransform)

        // apply transform to 'targetLandmarks' !!! - USE correct ids not the current ids!!!!!
        val alignedTargetLandmarks: IndexedSeq[Point[_3D]] = ids.map(i => model.mean.pointSet.point(PointId(i))).map(j => alignedTargetMesh.pointSet.findClosestPoint(j).point).toIndexedSeq

        // standard deviation for noise
        val littleNoise = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3))

        // training data
        val discreteTrainingData = (modelLandmarksIDs zip alignedTargetLandmarks).map { case (mId, pPt) => (mId, pPt, littleNoise) }.toIndexedSeq

        // trained statistical model
        val trainedModel: StatisticalMeshModel = model.posterior(discreteTrainingData)

        implicit val rng = scalismo.utils.Random(42)

        // sample on target mesh
        val targetSamples = UniformMeshSampler3D(alignedTargetMesh, samplesOnTargetMesh).sample.map(s => s._1)

        // closest point id's on mean mesh
        val meanMeshIds = targetSamples.map { s => trainedModel.mean.pointSet.findClosestPoint(s).id }

        // get sampled points on posterior's mean (by 'meanMeshIds')
        val meanMeshSamples = meanMeshIds.map(id => trainedModel.mean.pointSet.point(id))

        // method builds a posterior model and return it's mean mesh
        def fitModel(pointIds: IndexedSeq[PointId], candidateCorresp: Seq[Point[_3D]]): TriangleMesh3D = {
          // 'training' data is deformation field between pre selected landmarks and candidates on target mesh
          val trainingData = (pointIds zip candidateCorresp).map { case (mId, pPt) =>
            (mId, pPt, littleNoise)
          }
          val posterior = trainedModel.posterior(trainingData.toIndexedSeq)
          posterior.mean
        }

        def attributeCorrespondences(pts: Seq[Point[_3D]]): Seq[Point[_3D]] = {
          pts.map { pt => alignedTargetMesh.pointSet.findClosestPoint(pt).point }
        }

        def recursion(currentPoints: Seq[Point[_3D]], nbIterations: Int): TriangleMesh[_3D] = {
          //println("iterations left " + nbIterations)
          // find candidates on target mesh
          val candidates = attributeCorrespondences(currentPoints)

          // use the candidates as constraints for fitting
          val fit = fitModel(meanMeshIds, candidates)

          val newPoints = meanMeshIds.map(id => fit.pointSet.point(id))

          if (nbIterations > 0) {
            //Thread.sleep(100)
            recursion(newPoints, nbIterations - 1)
          }

          fit
        }

        // start recursive fitting
        val fit = recursion(meanMeshSamples, fittingIterations)
        val avg = MeshMetrics.avgDistance(alignedTargetMesh, fit)
        ((avg * 100).round) / 100.toDouble
      }

      // convert 'vtkPoints' to 'Seq[Landmark[_3D]'
      private def vtkPointsToSeqLandmark3D(points: vtkPoints, labels: Array[String]): Seq[Landmark[_3D]] = {
        val n = points.GetNumberOfPoints()
        var retLandmarks: Seq[Landmark[_3D]] = Seq[Landmark[_3D]]()

        for (i <- 0 until n) {
          retLandmarks = retLandmarks :+ Landmark(labels(i), Point3D(points.GetPoint(i)(0), points.GetPoint(i)(1), points.GetPoint(i)(2)))
        }

        retLandmarks
      }

  }

}