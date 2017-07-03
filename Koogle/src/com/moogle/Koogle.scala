package com.moogle

import java.net.URL
import java.util.Calendar

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet, ParMap, ParSet}
import scala.io.Source

/** Koogle - The Social Network Search Engine
  * Created by elmak & liorv.
  */
object Koogle {

  val DebugPrint = true
  val CrawlerSearchDepth = 2 // Highly demanding value
  val LoopRankDepth = 4
  val LogarithmicPhaseFactor = 0.85
  val PageOutLinksScore = 10.0

  def getPageHtml(url: String) = Source fromURL url.mkString

  def getLinksSet(url: String) = getLinks(url).asScala.par.map(_ absUrl "href").toSet

  def hasText(url: String, text: String) = getPage(url) text() contains text

  def showText(url: String, text: String) = println(getPage(url) text())

  def makeFull(url: String, callingSite: String) : String =  if (url contains "://" ) url else callingSite concat "/"  concat url

  def getPage(url: String): Document = try { (Jsoup connect url).get } catch { case e : Exception => (Jsoup connect "http://blank.org/").get} // blank page

  def getLinks(url: String) = try { getPage(url) select "a[href]" } catch { case e : Exception => new Elements()}

  type Condition = (Document) => Boolean

  def getAllPages(url: String, deep: Int, condition: Condition) : ParSet[PageData] = {
    getPages(url, deep, condition)
  }

  def checkAllPages(urls: List[String], deep: Int, condition: Condition) : ParSet[PageData] = {
    urls.par.flatMap(l => getPages(l, deep, condition)).toSet.par
  }

  def takeIf(condition: Condition, url: String) : ParSet[PageData] =  {
    val page = getPage(url) // val prevents multiple page downloads
    if (condition(page))
      new ParHashSet[PageData]() + new PageData(url, page, getLinksSet(url))
    else
      new ParHashSet[PageData]()
  }

  def violate(url: String): Boolean = {
    !((url contains ".htm") ||
      (url contains ".asp") ||
      (url contains ".php")) &&
      (url count(_ == '.')) > 2 || // filter only websites
      url.equals("https://twitter.com/") ||
      (
        "lang" ::
        "support.twitter" ::
        "twitter.com/account" ::
        "twitter.com/login" ::
        "twitter.com/privacy" ::
        "twitter.com/signup" ::
        "status.twitter" ::
        "dev.twitter" ::
        "twitter.com/about" ::
        Nil
      ).exists(x => url.toLowerCase().contains(x)) // twitter
  }

  def inRoots(url: String) : Boolean = CrawlerRoots.exists(l => url contains new URL(l).getHost)

  def getPages(url: String, deep: Int, condition: Condition, visitedLinks : HashSet[String] = new HashSet[String]) : ParSet[PageData] = {
    if ((visitedLinks contains url) || violate(url) || !inRoots(url)) return new ParHashSet[PageData]()
    if (!(visitedLinks contains url) && DebugPrint) println("+" + url)

    if (deep == 1) return takeIf(condition, url)

    if (DebugPrint) println(deep toString() concat ": " concat url)

    val links = getLinksSet(url) // val prevents multiple page downloads

    links.par.flatMap(l => {
      getPages(l, deep - 1, condition, visitedLinks.union(links) - l + url )
    }).union(takeIf(condition, url))
  }

  def createGraph(urls : ParSet[PageData]): ParMap[String, PageData] = {
    urls.map(p => {
      p.url -> p
    }).toMap
  }
  def CrawlerRoots =
    "https://twitter.com/Google" ::
    "https://twitter.com/netanyahu" ::
    "https://twitter.com/NASA" ::
    "https://twitter.com/BBCBreaking" ::
    Nil


  def search(query: String, depth: Int = CrawlerSearchDepth): ParSet[PageData] = {
    checkAllPages(CrawlerRoots, depth, (doc: Document) => {
      query.split(" ").exists(s => doc text() toLowerCase() contains s)
    })
  }

  def getLoopAmount(rootUrl: String, graph: ParMap[String, PageData], findUrl: String): Int = {
    def getLoopAmountRec(rootUrl: String, deep : Int): Int = {
      val pd = getOrNull(rootUrl, graph)
      if (deep == 1 || !graph.contains(rootUrl) || pd.url == null) return 0
      pd.outLinks.count(_.equals(findUrl)) + pd.outLinks.par.foldLeft(0)(_ + getLoopAmountRec(_, deep - 1))
    }
    getLoopAmountRec(rootUrl, LoopRankDepth)
  }

  def getOrNull(url: String, graph: ParMap[String, PageData]) = graph.getOrElse(url, new PageData(null, null, null))

  /* 1st ranking phase:  The graph loop-oriented ranking phase */
  def rankPage(graph: ParMap[String, PageData], pd: PageData, deep: Int, condition: Condition) : ParMap[String, Int] = {
    if (pd == null || pd.url == null) return new ParHashMap[String, Int]
    if (deep == 1) return new ParHashMap[String, Int]{(pd.url , getLoopAmount(pd.url, graph, pd.url))}

    pd.outLinks.map(s => s -> getOrNull(s, graph)).toMap.par.flatMap(
    l => { rankPage(graph, getOrNull(l._1, graph), deep - 1, condition)}) +
      (pd.url , getLoopAmount(pd.url, graph, pd.url))
  }

  /* 2nd ranking phase: The logarithmic phase, give less power to farther nodes  */
  def logarithmicRankTransform(graph: ParMap[String, Int]) : Double = {
    graph.toList.sortWith(_._2 > _._2).map(_._2).zipWithIndex.par.foldLeft(0.0){
      case (a, (i, v)) => a + v.toDouble * Math.pow(LogarithmicPhaseFactor, i)
    }
  }

  /* 3rd ranking phase: The graph loop-oriented ranking phase */
  def incomingRatio(url: String, graph : ParMap[String, PageData]) : Double = {
    graph.par.foldLeft(0.0){ case (a, (i, v)) => if (v.outLinks.contains(url)) a + PageOutLinksScore / v.outLinks.size else a }
  }

  def digitsOnly(x: String) = x forall Character.isDigit

  def isProfileOrTwitterPage(url : String) = url.replace("http://", "").replace("https://", "").count(_ == '/') == 1

  def isStatusPage(url : String) = url.contains("status")

  def main(args: Array[String]): Unit = {
    def getArgs : (String, Int) = {
        args.length match {
          case 1 => (args(0), CrawlerSearchDepth)
          case 2 => if (digitsOnly(args(1))) (args(0), args(1).toInt) else throw new IllegalArgumentException("The optional second parameter must be an integer value.")
          case _ => throw new IllegalArgumentException("The format is: \"<Search Query>\" <Optional: Search Depth>. Note that the query is between quotation marks.")
        }
    }

    if (DebugPrint) println("Search start: " + Calendar.getInstance().getTime.toString + System.currentTimeMillis())
    val g = createGraph(search(getArgs._1, getArgs._2))
    if (DebugPrint) println("Search End: " + Calendar.getInstance().getTime.toString + System.currentTimeMillis())

    val results = g.map(kv => {
      val r = rankPage(g, g.getOrElse(kv._1, null), 3, x => true)
      logarithmicRankTransform(r) + incomingRatio(kv._1, g) -> kv._1
    }).toList.sortWith(_._1 > _._1)

    println(logo)
    println("===========================================================================")
    println("============================= R E S U L T S ===============================")
    println("===========================================================================")
    println("====================>  Query: " + getArgs._1)
    println("Tweets:")
    results.filter(s => isStatusPage(s._2)).foreach{ case (r, u) => println(r + " " + u) }
    println("Profiles:")
    results.filter(s => isProfileOrTwitterPage(s._2)).foreach{ case (r, u) => println(r + " " + u) }
  }

  val logo = "              .-'''-.        .-'''-.                                       \n             '   _    \\     '   _    \\           .---.                     \n     .     /   /` '.   \\  /   /` '.   \\          |   |      __.....__      \n   .'|    .   |     \\  ' .   |     \\  '  .--./)  |   |  .-''         '.    \n .'  |    |   '      |  '|   '      |  '/.''\\\\   |   | /     .-''\"'-.  `.  \n<    |    \\    \\     / / \\    \\     / /| |  | |  |   |/     /________\\   \\ \n |   | ____`.   ` ..' /   `.   ` ..' /  \\`-' /   |   ||                  | \n |   | \\ .'   '-...-'`       '-...-'`   /(\"'`    |   |\\    .-------------' \n |   |/  .                              \\ '---.  |   | \\    '-.____...---. \n |    /\\  \\                              /'\"\"'.\\ |   |  `.             .'  \n |   |  \\  \\                            ||     ||'---'    `''-...... -'    \n '    \\  \\  \\                           \\'. __//                           \n'------'  '---'                          `'---'                            "
}
