import java.io.File
import java.io.FileFilter
import java.io.FilenameFilter
import scala.collection.immutable._
import scala.io.Source

/**
 * Very generic file utilities.
 */
object FileUtils {

  /**
   * Get a sequence of files from a directory and all sub-directories whose names match a condition.
   * 
   * @param dir    top-level directory
   * @param filter filter to run on the names of the files
   * 
   * @return sequence of files matching the condition
   */
  def listFilesRecursive(dir: File, filter: String => Boolean): Seq[File] = {
    assert(dir.isDirectory)
    val filenameFilter = new FilenameFilter { def accept(dir: File, name: String): Boolean = filter(name) }
    val dirFilter = new FileFilter { def accept(f: File): Boolean = f.isDirectory }
    
    // sub-directories of a top-level directory
    def subdirs(topDir: File): Seq[File] = {
      assert(topDir.isDirectory)
      refArrayOps(topDir.listFiles(dirFilter)).to[Seq]
    }
    
    // list only the matching files in the current directory
    def matchingFiles(topDir: File): Seq[File] = {
      assert(topDir.isDirectory)
      refArrayOps(topDir.listFiles(filenameFilter)).to[Seq]
    }
    
    def listFilesAccum(dir: File, accum: Seq[File]): Seq[File] = {
      val sd = subdirs(dir)
      val mf = matchingFiles(dir)
      if (sd.isEmpty) 
        accum ++ mf
      else
        accum ++ mf ++ sd.flatMap(listFilesAccum(_, Seq.empty[File]))
    }

    listFilesAccum(dir, Seq.empty[File])
  }

  /**
   * Get a sequence of files from a directory and all sub-directories whose names match a condition.
   * 
   * @param dir    top-level directory name as a string
   * @param filter filter to run on the names of the files
   * 
   * @return sequence of files matching the condition
   */
  def listFilesRecursiveStr(dir: String, filter: String => Boolean): Seq[File] = {
    val dirFile = new File(dir)
    assert(dirFile.isDirectory)
    listFilesRecursive(dirFile, filter)
  }
  
  /**
   * Reads an entire file into a string.
   * 
   * @param file file to read
   * @return the file as a string
   */
  def read(file: File): String = Source.fromFile(file).mkString("\n")
  
}
