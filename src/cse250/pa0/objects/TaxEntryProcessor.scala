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

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object TaxEntryProcessor {
  def sanitizeData(filename: String): ListBuffer[Array[String]]= {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()
    var count = 0
    var zip_index = 0
    var s = ListBuffer[String]()
    var clean_data = ListBuffer[Array[String]]()
    val remove_indexes = Array(1,2,8,9,10,11,12,13,14,21,22,23,25,32,33,34,40,41,42)
    for(line <- lines) {
      if (count == 0) {
        count += 1
        val ln = line.split(",")
        var cleaned_line = ListBuffer[String]()
        for (x <- 1 to ln.length) {
          if (remove_indexes.contains(x) != true) {
            cleaned_line += ln(x - 1)
          }
        }
        zip_index = cleaned_line.indexOf("ZIP CODE (5-DIGIT)")
        clean_data += cleaned_line.toArray
      } else {
        count += 1
        var cleaned_line = parseLine(line, remove_indexes)
        if(cleaned_line(zip_index) == ""){
          null
        } else{clean_data += cleaned_line.toArray}
      }
    }
    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))
    for(data <- clean_data){
      var line = data.mkString(",")
      outputFile.write(line + "\n")
    }
    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.
    // Close the files at the end.
    inputFile.close()
    outputFile.close()
    print(clean_data.length)
    clean_data
  }

  def computeMostExpensiveEntry(filename: String): TaxEntry = {
    var most_expensive = new TaxEntry
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    var count = 0
    var zip_index = 0
    var s = ListBuffer[String]()
    var clean_data = ListBuffer[Array[String]]()
    val remove_indexes = Array(1,2,8,9,10,11,12,13,14,21,22,23,25,32,33,34,40,41,42)
    for(line <- lines) {
      if (count == 0) {
        count += 1
        val ln = line.split(",")
        var cleaned_line = ListBuffer[String]()
        for (x <- 1 to ln.length) {
          if (remove_indexes.contains(x) != true) {
            cleaned_line += ln(x - 1)
          }
        }
        zip_index = cleaned_line.indexOf("ZIP CODE (5-DIGIT)")
        clean_data += cleaned_line.toArray
      } else {
        count += 1
        var cleaned_line = parseLine(line, remove_indexes)
        if(cleaned_line(zip_index) == ""){
          null
        } else{clean_data += cleaned_line.toArray}
      }
    }
    count = 0
    val val_index = clean_data.head.indexOf("TOTAL VALUE")
    var most_expensive_val = 0
    var most_expensive_ind = 0
    var header = ""
    for(data <- clean_data){
      if(count == 0){
         count += 1
        header = data.mkString(", ")
      } else{
        count += 1
        if(data(val_index) != "") {
          if (data(val_index).toInt > most_expensive_val) {
            most_expensive_val = data(val_index).toInt
            most_expensive_ind = clean_data.indexOf(data)
            most_expensive.infoMap.empty
            most_expensive.infoMap += (header -> data.mkString(", "))
          }
        }
      }
    }
    most_expensive
  }

  def computeOldestEntry(filename: String): TaxEntry = {
    var oldest = new TaxEntry
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    var count = 0
    var zip_index = 0
    var s = ListBuffer[String]()
    var clean_data = ListBuffer[Array[String]]()
    val remove_indexes = Array(1,2,8,9,10,11,12,13,14,21,22,23,25,32,33,34,40,41,42)
    for(line <- lines) {
      if (count == 0) {
        count += 1
        val ln = line.split(",")
        var cleaned_line = ListBuffer[String]()
        for (x <- 1 to ln.length) {
          if (remove_indexes.contains(x) != true) {
            cleaned_line += ln(x - 1)
          }
        }
        zip_index = cleaned_line.indexOf("ZIP CODE (5-DIGIT)")
        clean_data += cleaned_line.toArray
      } else {
        count += 1
        var cleaned_line = parseLine(line, remove_indexes)
        if(cleaned_line(zip_index) == ""){
          null
        } else{clean_data += cleaned_line.toArray}
      }
    }
    count = 0
    val val_index = clean_data.head.indexOf("YEAR BUILT")
    var oldest_val = 0
    var oldest_ind = 0
    var header = ""
    for(data <- clean_data){
      if(count == 0){
        count += 1
        header = data.mkString(", ")
      } else if(oldest_val == 0 &&  data(val_index) != ""){
          oldest_val = data(val_index).toInt
          oldest_ind = clean_data.indexOf(data)
          oldest.infoMap += (header -> data.mkString(", "))
          count += 1
        }else {
        count += 1
        if (data(val_index) != "" && data(val_index).toInt > 1700) {
          if (data(val_index).toInt < oldest_val) {
            oldest_val = data(val_index).toInt
            oldest_ind = clean_data.indexOf(data)
            oldest.infoMap.empty
            oldest.infoMap += (header -> data.mkString(", "))
          }
        }
      }
    }
    oldest
  }

  def parseLine(line: String, remove_indexes: Array[Int]): ListBuffer[String] = {
    var ln1 = line.replaceAll(""","""", ",@").replaceAll("""",""", "@,").replaceAll("""""""", "@").split(",", -1)
    var cleaned_line = ListBuffer[String]()
    var ln2 = ListBuffer[String]()
    for(s <- ln1){
      if(s.contains("@")){
        if(s.contains("(")){
          ln2 += """"""".concat(s.concat(", ").concat(ln1.last).replace("@", "")).concat(""""""")
        } else{
          if(ln1(ln1.indexOf(s) - 1).contains('@')){
            ln2 += """"""".concat(ln1(ln1.indexOf(s) - 1).concat(",").concat(s).replaceAll("@", "")).concat(""""""")
          }
        }
      } else if(s.contains(")")){
        null
      } else{ ln2 += s}
    }
    for(x <- 1 to ln2.length){
      if(remove_indexes.contains(x) != true){
        cleaned_line += ln2(x - 1)
      }
    }
    cleaned_line
  }
}