package com.moogle.downloader

import java.io.File
import java.net.URL
import com.moogle.downloader.HttpDownloader

/**
  * Created by el on 01/02/2017.
  */
object download {
  def write(path: String, txt: String): Unit = {
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Paths}

    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
  }
  def inputToFile(is: java.io.InputStream, f: java.io.File) {
    val in = scala.io.Source.fromInputStream(is)
    val out = new java.io.PrintWriter(f)
    try { in.getLines().foreach(out.println(_)) }
    finally { out.close }
  }
  def fileDownloader(url: String, filename: String) = {
    //new URL(url) #> new File(filename) !!
    val con = new URL(url).openConnection()
    con.addRequestProperty("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
    con.connect()
    Thread.sleep(500)
    inputToFile(con.getInputStream(), new File(filename))
    //con.getContent.toString #> new File(filename) !!

  }
  def i = 0
  def nums = 1 to 17
  def main(args: Array[String]): Unit =
  {
    nums.par.foreach(x =>
      //fileDownloader("http://www4.diburim.co.il/cgi-bin/forumsdb/intraforum_db.cgi?forum=8721/" + x + "&session=FFFF", "D:\\Backup\\Desktop\\F\\" + x +".htm")
      HttpDownloadUtility.downloadFile("http://www4.diburim.co.il/cgi-bin/forumsdb/intraforum_db.cgi?forum=8721/" + x + "&session=FFFF", "D:\\Backup\\Desktop\\F\\" + x +".htm")
    )

  }

}
