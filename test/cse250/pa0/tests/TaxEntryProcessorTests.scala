package cse250.pa0.tests

import cse250.pa0.objects.TaxEntryProcessor
import org.scalatest.FlatSpec

class TaxEntryProcessorTests extends FlatSpec {
  behavior of "TaxEntryProcessor.sanitizeData"

  "sanitizeData" should "return a cleaned csv file" in {
    assert(TaxEntryProcessor.sanitizeData("data/2017-2018_Assessment_Roll.csv")(27).length == 27)
    //assert(TaxEntryProcessor.computeMostExpensiveEntry("data/2017-2018_Assessment_Roll.csv").infoMap.head._2 == "3212341")
    assert(TaxEntryProcessor.computeOldestEntry("data/2017-2018_Assessment_Roll.csv").infoMap.head._2 == "4321432")
  }
}
