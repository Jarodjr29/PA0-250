/**
 * Main.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.objects

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "data/2017-2018_Assessment_Roll.csv"
    TaxEntryProcessor.sanitizeData(filename)
    println(TaxEntryProcessor.computeMostExpensiveEntry(filename+ "updated"))
    println(TaxEntryProcessor.computeOldestEntry(filename+"updated"))
  }
}
