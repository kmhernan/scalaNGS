/**
 * Package com.read.io
 *
 * @author Kyle Hernandez
 */

package com.read.io

import com.read._
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

      if(file.getName().endswith(".gz") 
        openGzipFileForReading(file)

      else
        new FileInputStream(file)
    }
    catch {
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe)
    }

  }

  def openGzipFileForReading(file: File): GZIPInputStream = {
    try
      new GZIPInputStream(new FileInputStream(file))
    catch
      case ioe: IOException => println("Error opening file: " + file.getName() + " " + ioe)

