/**
 * TaxEntryProcessor.scala
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

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.util.control.Breaks._
import scala.io.Source
import cse250.assignments.objects.TaxEntry

import scala.collection.mutable.ListBuffer

object TaxEntryProcessor {
  def sanitizeData(filename: String): ListBuffer[Array[String]] = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()

    var s = ListBuffer[String]()
    var clean_data = ListBuffer[Array[String]]()

    val remove_indexes = Array(1,2,8,9,10,11,12,13,14,21,22,23,25,32,33,34,40,41,42)

    for(line <- lines){
      var cleaned_line = ListBuffer[String]()
      var ln1 = line.replaceAll(""","""", ",@").replaceAll("""",""", "@,").replaceAll("""""""", "@").split(",", -1)
      var ln2 = ListBuffer[String]()
      for(s <- ln1){
        if(s.contains("@")){
          if(s == ln1.last){
            ln2 += s.replaceAll("@", "")
          } else{
            if(ln1(ln1.indexOf(s) - 1).contains('@')){
              ln2 += ln1(ln1.indexOf(s) - 1).concat(",").concat(s).replaceAll("@", "")
            }
          }
        } else{ ln2 += s}
      }
      for(x <- 1 to ln2.length){
        if(remove_indexes.contains(x) != true){
          cleaned_line += ln2(x - 1)
        }
      }
      //var ln2 = line.split(",",-1).toBuffer
     /* breakable {
        for (x <- ln2) {
          try {
            if(x == ln2.head){
              cleaned_line += x
            }else if (x == ln2.last) {
              cleaned_line += x.replaceAll("@", "")
            } else {
              /*val next_x_index = ln2.indexOf(x) +1
          val next_x = ln2(next_x_index)*/
              if (remove_indexes.contains(ln2.indexOf(x) - 1) != true) {
                if (x.contains("@") == true) {
                  var new_x = x.concat(",").concat(ln2(ln2.indexOf(x) + 1)).replace("@", "")
                  cleaned_line += new_x
                  ln2 -= ln2(ln2.indexOf(x) + 1)
                } else {
                  cleaned_line += x.replaceAll("@", "")
                }
              }
            }
          } catch {
            case ex: java.lang.NullPointerException => break()
          }
        }
      }*/
      clean_data += cleaned_line.toArray
    }
    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))

    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.


    // Close the files at the end.
    inputFile.close()
    outputFile.close()
    clean_data
  }

  def computeMostExpensiveEntry(filename: String): TaxEntry = {
    new TaxEntry
  }

  def computeOldestEntry(filename: String): TaxEntry = {
    new TaxEntry
  }
}
