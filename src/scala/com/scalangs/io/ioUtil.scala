/**
 * Package com.read.io
 *
 * @author Kyle Hernandez
 */

package com.scalangs.io

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

  def openFileForReading(file: File): InputStream = {
    try {
      if(file.getName().endsWith(".gz")){
        openGzipFileForReading(file)
      } else
        new FileInputStream(file)
    }
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 

  }

  def openGzipFileForReading(file: File): GZIPInputStream = {
    try
      new GZIPInputStream(new FileInputStream(file))
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 

  }

  def openFileForWriting(file: File): OutputStream = {
    try {
      /*if (file.getName().endsWith(".gz"))
        openGzipFileForWriting(file)
      else*/
      new FileOutputStream(file)
    }
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 
  
  }

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

  /**def openGzipFileForWriting(file: File): OutputStream = {
    try
      new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e => {
        println("Unhandled Exception!")
        e.printStackTrace()
        sys.exit(1)
      }
    } 
 
  }*/
     
}

