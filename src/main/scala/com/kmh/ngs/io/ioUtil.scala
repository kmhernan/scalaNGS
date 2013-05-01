/**
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *
 * For more information, please refer to <http://unlicense.org/>
 *
 */

package com.kmh.ngs.io

import java.io._
import java.util.zip.GZIPInputStream
import org.eintr.loglady.Logging
import scala.io.BufferedSource

/**
 * A class for utility IO methods around [[java.io._]] or [[scala.io.BufferedSource]]
 *
 * @author Kyle Hernandez
 */

class IoUtil extends Logging {

  /**
   * Checks the existance and availability of a file. Thows exceptions when false
   *
   * @param file the file to check of type File
   */
  def assertFileIsReadable(file: File): Unit = {
    if (!file.exists())
      log.error(throw new IOException("Cannot read non-existent file: " + file.getAbsolutePath()))
  }

  /**
   * Opens a file for reading. Currently supports .gz files. Throws exceptions when
   * the file can't be opened.
   * 
   * @param file the file to open
   * @return InputStream
   * @throws [[IOException]]
   */
  def openFileForReading(file: File): InputStream = {
    try {
      if(file.getName().endsWith(".gz")){
        openGzipFileForReading(file)
      } else
        new FileInputStream(file)
    }
    catch {
      case ioe: IOException => log.error("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => log.error("Unhandled Exception!\n" + e.printStackTrace()); sys.exit(1);
    } 
  }

  /**
   * Opens a GZ file. Throws an exception when it can't open.
   *
   * @param file the gz file
   * @return GZIPInputStream
   * @throws [[IOException]]
   */
  def openGzipFileForReading(file: File): GZIPInputStream = {
    try
      new GZIPInputStream(new FileInputStream(file))
    catch {
      case ioe: IOException => log.error("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => log.error("Unhandled Exception!\n" + e.printStackTrace()); sys.exit(1);
    } 
  }

  /**
   * Opens a file for [[scala.io.BufferedSource]] reading. Currently supports .gz files. Throws exceptions when
   * the file can't be opened.
   * 
   * @param file the [[java.io.File]] to open
   * @return [[scala.io.BufferedSource]]
   * @throws [[IOException]]
   */
  def openFileForBufferedSource(file: File): BufferedSource = {
    try {
      if(file.getName().endsWith(".gz")){
        openGzipFileForBufferedSource(file)
      } else
        new BufferedSource(new FileInputStream(file))
    }
    catch {
      case ioe: IOException => log.error("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => log.error("Unhandled Exception!\n" + e.printStackTrace()); sys.exit(1);
    } 
  }

  /**
   * Opens a GZ file. Throws an exception when it can't open.
   *
   * @param file the gz file
   * @return [[scala.io.BufferedSource]] 
   * @throws [[IOException]]
   */
  def openGzipFileForBufferedSource(file: File): BufferedSource = {
    try
      new BufferedSource(new GZIPInputStream(new FileInputStream(file)))
    catch {
      case ioe: IOException => log.error("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => log.error("Unhandled Exception!\n" + e.printStackTrace()); sys.exit(1);
    } 
  }

  /**
   * Opens a file for writing. Throw exception if unable to open stream.
   * Curerntly doesn't support zipped files.
   *
   * @param file the file to output to
   * @return OutputStreamWriter
   * @throws [[IOException]]
   */
  def openFileForWriting(file: File): OutputStreamWriter = {
    try
      new OutputStreamWriter(new FileOutputStream(file))
    catch {
      case ioe: IOException => log.error("Error opening file: " + file.getName() + " " + ioe); sys.exit(1);
      case e: Throwable => log.error("Unhandled Exception!\n" + e.printStackTrace()); sys.exit(1);
    } 
  }

  /**
   * Opens a Buffered reader
   *
   * @param file the file to create a BufferedReader from
   * @return BufferedReader
   */
  def openFileForBufferedReading(file: File): BufferedReader = {
    new BufferedReader(new InputStreamReader(openFileForReading(file)))
  }

  /**
   * Throws IOException on error unlike PrintWriter
   *
   *
  def openFileForBufferedWriting(file: File): BufferedWriter = {
    new BufferedWriter(new OutputStreamWriter(openFileForWriting(file)))
  }*/

  def closer(stream: Closeable): Unit = {
      stream.close()
  }

  def closerSource(stream: BufferedSource): Unit = {
      stream.close()
  }
}
