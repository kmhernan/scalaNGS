
package com.ngs.io

import java.io._
import java.util.zip.GZIPInputStream

/**
 * A class for utility IO methods around java.io
 *
 * @author Kyle Hernandez
 */

class IoUtil {

  /**
   * Checks the existance and availability of a file. Thows exceptions when false
   *
   * @param file the file to check of type File
   */
  def assertFileIsReadable(file: File): Unit = {

    if (!file.exists())
      throw new IOException("Cannot read non-existent file: " + file.getAbsolutePath())

  }

  /**
   * Opens a file for reading. Currently supports .gz files. Throws exceptions when
   * the file can't be opened.
   * 
   * @param file the file to open
   * @returns InputStream
   */
  def openFileForReading(file: File): InputStream = {

    try {
      if(file.getName().endsWith(".gz")){
        openGzipFileForReading(file)
      } else
        new FileInputStream(file)
    }
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 

  }

  /**
   * Opens a GZ file. Throws an exception when it can't open.
   *
   * @param file the gz file
   * @returns GZIPInputStream
   */
  def openGzipFileForReading(file: File): GZIPInputStream = {
 
    try
      new GZIPInputStream(new FileInputStream(file))
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 

  }

  /**
   * Opens a file for writing. Throw exception if unable to open stream.
   * Curerntly doesn't support zipped files.
   *
   * @param file the file to output to
   * @returns OutputStream
   */
  def openFileForWriting(file: File): OutputStream = {

    try
      new FileOutputStream(file)
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 
  
  }

  /**
   * Opens a Buffered reader
   *
   * @param file the file to create a BufferedReader from
   * @returns BufferedReader
   */
  def openFileForBufferedReading(file: File): BufferedReader = {

    new BufferedReader(new InputStreamReader(openFileForReading(file)))
  
  }

  /**
   * Throws IOException on error unlike PrintWriter
   *
   */
  def openFileForBufferedWriting(file: File): BufferedWriter = {

    new BufferedWriter(new OutputStreamWriter(openFileForWriting(file)))

  }

  def close(stream: Any): Unit = {
    stream match {
      case BufferedReader => stream.close()
      case InputStreamReader => stream.close()
    } 

}
