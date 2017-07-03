package com.moogle

import org.jsoup.nodes.Document

import scala.collection.parallel.immutable.ParSet

/**
  * Created by elmak & liorv.
  */

class PageData (val url: String, val doc: Document, val outLinks: ParSet[String]) { }
