// java
import java.io.{BufferedReader, File, InputStreamReader, PrintStream, PrintWriter}
import java.net.{ServerSocket, Socket}

// hdf
import ncsa.hdf.hdf5lib.H5
import ncsa.hdf.hdf5lib.HDF5Constants

// scala
import scala.io.Source
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.language.implicitConversions

// scalismo
import scalismo.utils.MeshConversion
import scalismo.utils.Random
import scalismo.common._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.statisticalmodel._
import scalismo.io.{StatismoIO, LandmarkIO, MeshIO}

// vtk
import vtk.{vtkPoints, vtkPolyDataWriter, vtkPolyData, vtkTransform, vtkTransformPolyDataFilter}

// BoneHostServer
import Helper.Fitting

object BoneHostServer {

  // server the program is running on
  //var BoneHostServerURL = "http://185.16.60.56" // BoneHost-Server
  var BoneHostServerURL = "http://localhost" // local

  // port the server is running on
  var BoneHostServerPort = 16181

  // ### 'DataURL': WebServer or local path which holds the data
  //var BoneHostDataURL = "/var/www/Data" // BoneHost-Server
  var BoneHostDataURL = "D:/Projekte/2018/BoneHostServer" // local

  // shared file handle
  var file_id = -1

  // fitting class
  val fitting: Fitting = new Fitting()

  // write coordinates and ids to file
  private def SaveCsvAscii(filePath: String, coordinates: Array[Float]): Unit = {
    val pw = new PrintWriter(filePath)
    var j = 1

    println("anatomical landmark file " + filePath + " saved!")

    // write data
    val dim = coordinates.size / 3

    for (i <- 0 until dim) {
      pw.write(i + "," + coordinates(i * 3) + "," + coordinates((i * 3) + 1) + "," + coordinates((i * 3) + 2) + "\n")
    }

    pw.close
  }

  // write labels, coordinates and ids to file
  private def SaveCsvAscii(filePath: String, coordinates: Array[Float], ids: Array[Int]): Unit = {
    val pw = new PrintWriter(filePath)
    var j = 1

    println("anatomical landmark file " + filePath + " saved!")

    // write data
    val dim = coordinates.size / 3

    for (i <- 0 until dim) {
      pw.write(i + "," + coordinates(i * 3) + "," + coordinates((i * 3) + 1) + "," + coordinates((i * 3) + 2) + "," + ids(i) + "\n")
    }

    pw.close
  }

  // write labels, coordinates and ids to file
  private def SaveCsvAscii(filePath: String, labels: Array[String], coordinates: Array[Float], ids: Array[Int]): Unit = {
    val pw = new PrintWriter(filePath)
    var j = 1

    // write data
    val dim = coordinates.size / 3

    for (i <- 0 until dim) {
      //println(labels(i) + "," + coordinates(i * 3) + "," + coordinates((i * 3) + 1) + "," + coordinates((i * 3) + 2) + "," + ids(i))
      pw.write(labels(i) + "," + coordinates(i * 3) + "," + coordinates((i * 3) + 1) + "," + coordinates((i * 3) + 2) + "," + ids(i) + "\n")
    }

    pw.close

    println("anatomical landmark file " + filePath + " saved!")
  }

  private def WaitForNewConnection(server: ServerSocket) {

    // blocking until client connects
    val socket: Socket = server.accept()
    println("client connected!")

    val thread = Future {

      println("thread started!")

      // input stream
      val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

      // one line of input stream
      var data = in.readLine()

      if (data == null) {
        println("data is null!")
        return; 
      }

      // check for post data (in first line)
      val isPost: Boolean = data.startsWith("POST");

      // content length - must be known to read post message
      var contentLength: Int = 0

      // model
      var model: StatisticalMeshModel = null

      // mesh
      var mesh: vtkPolyData = null

      // model's path
      var modelName: String = ""

      // landmark name and coordinates
      var landmarkName: String = ""

      // process http request header line by line ...
      // ... as long as data come's in and there is no empty line (length of data is zero)
      data = in.readLine();
      while ( (data != null) && (data.length != 0) ) {

        println(data)

        // extract file name from message
        if (data.contains("Mean") == true) {
          println("creating mean mesh ...")

          modelName = data.substring(6, data.length)

          // load 'group' data
          val (landmarks, ids, labels) = loadGroupData(modelName)

          // load statistical model
          model = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelName + ".h5")).get

          // extract model's mean
          val mean = model.mean

          // create anatomical landmarks by id (mean)
          SaveAnatomicalLandmark(modelName,"landmarks", mean, landmarks, ids, labels)

          // convert to vtkPolyData
          mesh = MeshConversion.meshToVtkPolyData(mean)

        } else if (data.contains("Random") == true) {
          println("creating random mesh ...")

          modelName = data.substring(8, data.length)

          // load 'group' data
          val (landmarks, ids, labels) = loadGroupData(modelName)

          // load statistical model
          model = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelName + ".h5")).get

          // random and extract mesh from model
          implicit val rng = Random(System.currentTimeMillis())

          // sample
          val sample = model.sample()

          // create anatomical landmarks by id (sample)
          SaveAnatomicalLandmark(modelName,"landmarks", sample, landmarks, ids, labels)

          // convert to vtkPolyData
          mesh = MeshConversion.meshToVtkPolyData(sample)

        } else if (data.contains("ModelFitting") == true) {
          println("creating model fit ...")

          // selected model
          val modelName = data.substring(14, data.length)

          //println(BoneHostDataURL + File.separator + "Temp" + File.separator + modelName + ".vtk")

          // load target mesh and target landmarks (based on selected model)
          val targetMesh = MeshIO.readMesh(new File(BoneHostDataURL + File.separator + "Temp" + File.separator + modelName + ".vtk")).get
          val targetLandmarks = LandmarkIO.readLandmarksCsv[_3D](new File(BoneHostDataURL + File.separator + "Temp" + File.separator + modelName + "-landmarks.csv")).get

          // asian / caucasian / female / male?
          var pos = modelName.indexOf("Female")
          if (pos < 0) {
            pos = modelName.indexOf("Male")
          }

          var modelNameFemaleAsian: String = data.substring(14, pos + 14) + "FemaleAsian1"
          var modelNameFemaleCaucasian: String = data.substring(14, pos + 14) + "FemaleCaucasian1"

          var modelNameMaleAsian: String = data.substring(14, pos + 14) + "MaleAsian1"
          var modelNameMaleCaucasian: String = data.substring(14, pos + 14) + "MaleCaucasian1"

          // ### FemaleAsian ###
          // load 'group' data of modelNameFemaleAsian model
          val (landmarksFemaleAsian, idsFemaleAsian, labelsFemaleAsian) = loadGroupData(modelNameFemaleAsian)

          // load statistical model
          val modelFemaleAsian = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelNameFemaleAsian + ".h5")).get

          // fitting asian model to target mesh
          println("fitting target mesh to female asian model ...")
          val rmsFemaleAsian = fitting.fit(modelFemaleAsian, targetMesh, targetLandmarks, landmarksFemaleAsian, idsFemaleAsian, labelsFemaleAsian)
          println("fitting done - rms = " + rmsFemaleAsian)

          // FemaleCaucasian ###
          // load 'group' data of female caucasian model
          val (landmarksFemaleCaucasian, idsFemaleCaucasian, labelsFemaleCaucasian) = loadGroupData(modelNameFemaleCaucasian)

          // load statistical model
          val modelFemaleCaucasian = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelNameFemaleCaucasian + ".h5")).get

          // fitting caucasian model to target mesh
          println("fitting target mesh to female caucasian model ...")
          val rmsFemaleCaucasian = fitting.fit(modelFemaleCaucasian, targetMesh, targetLandmarks, landmarksFemaleCaucasian, idsFemaleCaucasian, labelsFemaleCaucasian)
          println("fitting done - rms = " + rmsFemaleCaucasian)

          // ### MaleAsian ###
          // load 'group' data of modelNameFemaleAsian model
          val (landmarksMaleAsian, idsMaleAsian, labelsMaleAsian) = loadGroupData(modelNameMaleAsian)

          // load statistical model
          val modelMaleAsian = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelNameMaleAsian + ".h5")).get

          // fitting asian model to target mesh
          println("fitting target mesh to male asian model ...")
          val rmsMaleAsian = fitting.fit(modelMaleAsian, targetMesh, targetLandmarks, landmarksMaleAsian, idsMaleAsian, labelsMaleAsian)
          println("fitting done - rms = " + rmsMaleAsian)

          // MaleCaucasian ###
          // load 'group' data of Male caucasian model
          val (landmarksMaleCaucasian, idsMaleCaucasian, labelsMaleCaucasian) = loadGroupData(modelNameMaleCaucasian)

          // load statistical model
          val modelMaleCaucasian = StatismoIO.readStatismoMeshModel(new File(BoneHostDataURL + File.separator + "Models" + File.separator + modelNameMaleCaucasian + ".h5")).get

          // fitting caucasian model to target mesh
          println("fitting target mesh to male caucasian model ...")
          val rmsMaleCaucasian = fitting.fit(modelMaleCaucasian, targetMesh, targetLandmarks, landmarksMaleCaucasian, idsMaleCaucasian, labelsMaleCaucasian)
          println("fitting done - rms = " + rmsMaleCaucasian)

          // write results to disc
          val writer = new PrintWriter(new File(BoneHostDataURL + File.separator + "Temp" + File.separator + modelName + "-fitting.csv"))
          writer.write(modelNameFemaleAsian + "," + rmsFemaleAsian + "\n")
          writer.write(modelNameFemaleCaucasian + "," + rmsFemaleCaucasian + "\n")
          writer.write(modelNameMaleAsian + "," + rmsMaleAsian + "\n")
          writer.write(modelNameMaleCaucasian + "," + rmsMaleCaucasian + "\n")
          writer.close()

        } else if (data.contains("Content-Length")) {

          contentLength = data.substring(16, data.length).toInt

        } else if (data.contains("Landmark")) {

          // request header 'Landmark' indicates anatomical landmarks in body text
          landmarkName = data.substring(10, data.length)

        } else {
          //println("unknown request!")
        }

	data = in.readLine();

      }

      // former loop ends if an empty line (for data) comes in (this indicates following body/post message)
      if (isPost) {
        var body = ""
        for (i <- 0 until contentLength) {
          var c: Char = in.read().toChar
          body += c
        }
        print(body)

        // for now body will be thought to be landmarks ...
        UpdateLandmark(BoneHostDataURL + File.separator + "Temp" + File.separator + landmarkName + ".csv", body)

      }

      // ### MESH ###
      if (mesh != null) {
        val writer: vtkPolyDataWriter = new vtkPolyDataWriter()
        // save in ascii vtk format (only supported by client yet)
        writer.SetFileName(BoneHostDataURL + File.separator + "Temp" + File.separator + modelName + ".vtk")
        writer.SetInputData(mesh)
        writer.SetFileTypeToASCII()
        writer.Write()
        println("mesh created!")
      }

      // output stream
      val out = new PrintStream(socket.getOutputStream())

      // servers response to client (see http protocol, CORS, request header)
      out.print("HTTP/1.1 200 OK\r\n")
      out.print("Content-Type: text/plain\r\n")
      out.print("Access-Control-Allow-Origin: *\r\n")
      out.print("Connection: keep alive\r\n")
      out.print("Access-Control-Allow-Headers: Anatomy, Content-Type, Content-Length, Landmark, EthnicGroup, Gender, Mean, Random, ModelFitting, Side, Study\r\n\r\n")

      // send message and close socket
      out.flush()
      socket.close()
    }

    thread.onComplete {
      case Success(_) => println("thread finished!")
      case Failure(e) => e.printStackTrace
    }

  }

  // writes anatomical landmarks found by id from mesh to disc
  private def SaveAnatomicalLandmark(fileName: String, landmarkName: String, mesh: TriangleMesh[_3D], landmarks: vtkPoints, ids: Array[Int], labels: Array[String]) = {
    // ### LANDMARKS ###
    if ((landmarks != null) && (ids != null) && (labels != null)) {

      var pointData: vtkPoints = new vtkPoints()

      for (i <- 0 until ids.length) {
        val v = ids(i)
        val p = mesh.pointSet.point(PointId(v))
        pointData.InsertNextPoint(p.toArray)
      }

      // transform landmarks to unity system
      var landmarksTransformed = transformLandmarks(pointData)

      // save (transformed) landmarks in csv ascii format
      SaveCsvAscii(BoneHostDataURL + File.separator + "Temp" + File.separator + fileName + "-" + landmarkName + ".csv",
        labels,
        landmarksTransformed,
        ids
      )

      // clean up
      pointData.Delete()
    } else {
      println("Anatomical landmarks, id's oder labels's could not be created!")
    }
  }

  private def loadGroupData(modelName: String): (vtkPoints, Array[Int], Array[String]) = {

    // open hdf5 file
    openFile(BoneHostDataURL + File.separator + "Models" + File.separator + modelName + ".h5")

    // get coordinates, id's and labels for anatomical landmarks stored in 'group'
    val landmarks: vtkPoints = getAnatomicalLandmarks("/" + "thesis" + "/" + "anatomicalLandmarks")
    val ids: Array[Int] = getAnatomicalLandmarksId("/" + "thesis" + "/" + "anatomicalLandmarksId")
    val labels: Array[String] = getAnatomicalLandmarksLabel("/" + "thesis" + "/" + "anatomicalLandmarksLabel")

    // close hdf5 file
    closeFile()

    return (landmarks, ids, labels)
  }

  private def getAnatomicalLandmarksLabel(datasetPath: String): Array[String] = {

    if (!existsDataset(datasetPath)) {
      println("dataset '" + datasetPath + "' does not exists!")
      return null
    }

    var dataset_id = -1
    var memspace_id = -1
    var filetype_id = -1
    var memtype_id = -1

    var str_data: Array[String] = Array[String]()

    try
      dataset_id = H5.H5Dopen(file_id, datasetPath)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // Get dataset space.
    try
      memspace_id = H5.H5Dget_space(dataset_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // dataset's rows
    var dim0: Long = H5.H5Sget_simple_extent_npoints(memspace_id)

    // get the dataset type
    try
      filetype_id = H5.H5Dget_type(dataset_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // get the size of the dataset type.
    // +1 make room for null terminator.
    var dim1: Int = H5.H5Tget_size(filetype_id) + 1

    // allocate space for dataset.
    var s: Int = dim0.toInt * dim1
    var data = new Array[Byte](s)

    // create the memory dataset type.
    try {
      memtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
      H5.H5Tset_size(memtype_id, dim1)
    } catch {
      case e: Exception => println(e.printStackTrace())
    }

    // read dataset.
    try {
      if ((dataset_id >= 0) && (memtype_id >= 0))
        H5.H5Dread(dataset_id, memtype_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, data)
    } catch {
      case e: Exception => println(e.printStackTrace())
    }

    //data.map(i => println(i.toChar.toString))

    // extract string by new line separator '\n'
    var index = 0
    var tmpString = ""

    for (i <- 0 until s) {

      if (data(i).toChar == '\u0000') {
        if (tmpString != "") {
          str_data = str_data :+ tmpString
          tmpString = ""
        }
      } else {
        tmpString += data(i).toChar.toString
      }

      index += 1
    }

    //str_data.map(i => println(i))

    // clean up
    try {
      if (dataset_id >= 0) {
        H5.H5Dclose(dataset_id)
        dataset_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Cannot close the dataset: " + ex.getMessage())
    }

    try {
      if (memspace_id >= 0) {
        H5.H5Sclose(memspace_id)
        memspace_id = -1
      }
    }
    catch {
      case e: Exception =>
        println("Cannot close the property list: " + e.getMessage())
    }

    try {
      if (memtype_id >= 0) {
        H5.H5Tclose(memtype_id)
        memtype_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Cannot close the memtype: " + ex.getMessage())
    }

    try {
      if (filetype_id >= 0) {
        H5.H5Tclose(filetype_id)
        filetype_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Cannot close the filetype: " + ex.getMessage())
    }

    return str_data
  }

  private def getAnatomicalLandmarksId(datasetPath: String): Array[Int] = {

    if (!existsDataset(datasetPath)) {
      println("Dataset '" + datasetPath + "' does not exists!")
      return null
    }

    var dataset_id = -1
    var dataspace_id = -1

    // Open dataset using the default properties.
    try {
      if (file_id >= 0)
        dataset_id = H5.H5Dopen(file_id, datasetPath)
    } catch {
      case e: Exception => println(e.printStackTrace())
    }

    // Get dataspace and allocate memory for read buffer.
    try
      if (dataset_id >= 0)
        dataspace_id = H5.H5Dget_space(dataset_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // number of cells
    var dim0: Long = 0

    try
      if (dataspace_id >= 0)
        dim0 = H5.H5Sget_simple_extent_npoints(dataspace_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // data
    var data = new Array[Int](dim0.toInt)

    // Read the data using the default properties.
    try {
      if (dataset_id >= 0)
        H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, data)
    } catch {
      case e: Exception => println(e.printStackTrace())
    }

    // Output the data to the screen.
    //data.map(i => println(i))

    // clean up
    try {
      if (dataspace_id >= 0) {
        H5.H5Sclose(dataspace_id)
        dataspace_id = -1
      }
    } catch {
      case ex: Exception => println("Cannot close the dataspace: " + ex.getMessage())
    }

    try {
      if (dataset_id >= 0) {
        H5.H5Dclose(dataset_id)
        dataset_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Cannot close the dataset: " + ex.getMessage())
    }

    return data
  }

  private def getAnatomicalLandmarks(datasetPath: String): vtkPoints = {

    if (!existsDataset(datasetPath)) {
      println("Dataset '" + datasetPath + "' does not exists!")
      return null
    }

    var dataset_id = -1
    var dataspace_id = -1
    var dim0: Long = 0

    // Open an existing dataset.
    try
      if (file_id >= 0)
        dataset_id = H5.H5Dopen(file_id, datasetPath)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // Get dataspace and allocate memory for read buffer.
    try
      if (dataset_id >= 0)
        dataspace_id = H5.H5Dget_space(dataset_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // sometime crashes occured for some reason !?
    //dim0 = H5.H5Sget_simple_extent_npoints(dataspace_id)

    try
      if (dataspace_id >= 0)
        dim0 = H5.H5Sget_simple_extent_npoints(dataspace_id)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    var data = new Array[Float](dim0.toInt)

    // Read data.
    try
      if (dataset_id >= 0) H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_FLOAT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, data)
    catch {
      case e: Exception => println(e.printStackTrace())
    }

    // Output the data to the screen.
    //data.map(i => println(i))

    // clean up
    try {
      if (dataspace_id >= 0) {
        H5.H5Sclose(dataspace_id)
        dataspace_id = -1
      }
    } catch {
      case ex: Exception => println("Cannot close the dataspace: " + ex.getMessage())
    }

    try {
      if (dataset_id >= 0) {
        H5.H5Dclose(dataset_id)
        dataset_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Cannot close the dataset: " + ex.getMessage())
    }

    // return vtk points instead

    var pointData: vtkPoints = new vtkPoints()
    for (i <- 0 until (data.length / 3)) {
      pointData.InsertNextPoint(data(i * 3), data((i * 3) + 1), data((i * 3) + 2))
    }

    //return data
    return pointData
  }

  private def openFile(fName: String){
    try {
      // Open file using the default properties.
      file_id = H5.H5Fopen(fName, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT)
    } catch {
      case ex: Exception =>
        println("Not able to open " + fName + ": ")
    }
  }

  private def closeFile() {
    try {
      if (file_id >= 0) {
        H5.H5Fflush(file_id, HDF5Constants.H5F_SCOPE_LOCAL)
        H5.H5Fclose(file_id)
        file_id = -1
      }
    } catch {
      case ex: Exception =>
        println("Not able to close the file: " + ex.getMessage())
    }
  }

  private def existsDataset(dsPath: String): Boolean = {
    var result: Boolean = true
    var dataset_id = -1
    try {
      dataset_id = H5.H5Dopen(file_id, dsPath)
    } catch {
      case ex: Exception =>
        result = false
    }
    finally {
      try {
        if (dataset_id >= 0) {
          H5.H5Dclose(dataset_id)
          dataset_id = -1
        }
      } catch {
        case ex: Exception =>
          println("Cannot close the dataset: " + ex.getMessage())
      }
    }
    return result
  }

  private def transformMesh(mesh: vtkPolyData): vtkPolyData = {

    // consider unity's left handed coordinate system (flip x coordinates)
    val transform: vtkTransform = new vtkTransform()

    //transform.Scale(1, 1, 1)
    transform.RotateX(-90)

    val transformFilter: vtkTransformPolyDataFilter = new vtkTransformPolyDataFilter()

    // transform mesh
    transformFilter.SetTransform(transform)
    transformFilter.SetInputData(mesh)
    transformFilter.Update()
    var meshTransformed = transformFilter.GetOutput()

    return (meshTransformed)
  }

  private def transformLandmarks(landmarks: vtkPoints): Array[Float] = {

    // consider unity's left handed coordinate system (flip x coordinates)
    val transform: vtkTransform = new vtkTransform()

    // identity matrix will do ... nothing
    transform.Identity()

    // unity system expectgs a rotation around x-axis of 90 degrees
    //transform.RotateX(-90)

    val transformFilter: vtkTransformPolyDataFilter = new vtkTransformPolyDataFilter()

    // transform landmarks
    val pdata: vtkPolyData = new vtkPolyData()

    pdata.SetPoints(landmarks)
    transformFilter.SetTransform(transform)
    transformFilter.SetInputData(pdata)
    transformFilter.Update()

    var landmarksTransformed = transformFilter.GetOutput()

    // reconvert to Array[Float]
    var landmarksTransformedF: Array[Float] = new Array[Float](landmarksTransformed.GetPoints().GetNumberOfPoints() * 3)
    var index = 0

    for (i <- 0 until landmarksTransformed.GetPoints().GetNumberOfPoints()) {
      val p = landmarksTransformed.GetPoints().GetPoint(i)

      landmarksTransformedF(index) = p(0).toFloat
      landmarksTransformedF(index + 1) = p(1).toFloat
      landmarksTransformedF(index + 2) = p(2).toFloat

      index += 3
    }

    return (landmarksTransformedF)

  }

  private def UpdateLandmark(landmarkFile: String, landmarkData: String): Unit = {

    // split new landmark data by space
    val landmarkDataSplit = landmarkData.split(',')
    var labels: Array[String] = Array[String]()

    var coordinates: Array[Float] = Array[Float]()

    for (i <- 0 until landmarkData.size) {
      coordinates :+ landmarkData(i)
    }

    coordinates = landmarkDataSplit.map(i => i.toFloat)

    var ids: Array[Int] = Array[Int]()

    for (line <- Source.fromFile(landmarkFile).getLines) {
      val out: Array[String] = line.toString.split(",")

      labels = labels :+ out(0)

      // if id's were found reuse them, if not set each value to zero
      if (out.size > 3)
        ids = ids :+ out(4).toInt
      else
        ids = ids :+ 0
    }

    SaveCsvAscii(landmarkFile, labels, coordinates, ids)
  }

  def main(args: Array[String]): Unit = {

    // adjust parameter
    if (args.length == 3) {
      BoneHostServerURL = args(0)
      BoneHostServerPort = args(1).toInt
      BoneHostDataURL = args(2)
    } else {
      println("Usage: [BoneHostServerURL] [BoneHostServerPort] [BoneHostDataURL]")
      System.exit(0)
    }

    // init scalismo
    scalismo.initialize()

    // start server
    val server = new ServerSocket(BoneHostServerPort)
    println("listening on port " + BoneHostServerPort)

    // listen on predefined port, several clients may connect
    while (true) {
      WaitForNewConnection(server)
    }

  }

}
