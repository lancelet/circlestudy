import java.io.File

object HoofEventComparison {
  def main(args: Array[String]): Unit = {
    
    // Banner
    println("-----------------------")
    println("| HoofEventComparison |")
    println("-----------------------")
    
    val dataDir: File = new File("./data")
    
    // Read in kinematic events
    print("Reading kinematic events...")
    val kinematicEvents: Seq[HoofKinematicEvent] = KinematicEvents.fromDirectory(dataDir)
    print(" DONE\n")
    println(s"${kinematicEvents.length} raw kinematic events identified")

    // Read in kinetic events
    print("Reading kinetic events...")
    val kineticEvents: Seq[KineticEvent] = KineticEvents.fromDirectory(dataDir)
    print(" DONE\n")
    println(s"${kineticEvents.length} raw kinetic events identified (including duplicates across plates)")
    
    // Filter kinetic events so that events for the same trial which occur close together in time are removed
    val minTimeOverlap: Double = 0.01  // units: seconds
    print("Filtering kinetic events to remove those that overlap closely in time...")
    val kineticEventsNoTimeOverlap: Seq[KineticEvent] = {
      def accum(kes: Seq[KineticEvent], rem: Seq[KineticEvent]): Seq[KineticEvent] = {
        if (rem.isEmpty) kes
        else {
          val curEvent = rem.head
          val newRem   = rem.tail
          val overlappingEvent = kes.find(ke => math.abs(curEvent.time - ke.time) <= minTimeOverlap)
          val newKes = overlappingEvent match {
            case Some(_) => kes
            case None    => curEvent +: kes 
          }
          accum(newKes, newRem)
        }
      }
      accum(List.empty[KineticEvent], kineticEvents)
    }
    print(" DONE\n")
    val nRemovedDueToTimeOverlap = kineticEvents.length - kineticEventsNoTimeOverlap.length
    println(s"$nRemovedDueToTimeOverlap kinetic events removed due to overlapping by <= $minTimeOverlap seconds")
    println(s"${kineticEventsNoTimeOverlap.length} kinetic events left")
    
    // Match up kinetic events with closest kinematic events
    print("Matching kinetic and kinematic events...")
    val maxAcceptableTimeDifference: Double = 0.2 // seconds
    val matchEvents: Seq[(KineticEvent, HoofKinematicEvent)] = {
      val closestKinematicEvents: Seq[Option[(KineticEvent, HoofKinematicEvent)]] = 
        for (kineticEvent <- kineticEventsNoTimeOverlap) yield {
          // only look at the kinematic events from THIS trial, and which match ON/OFF contact conditions
          val trialKinematicEvents = kinematicEvents.filter ( e => 
            e.trial == kineticEvent.trial && e.onoff == kineticEvent.onoff
          )
          // find the kinematic even which matches most closely in time
          val kinematicEventTimeDeltas = trialKinematicEvents.map(e => math.abs(e.time - kineticEvent.time))
          val closestKinematicEvent = kinematicEventTimeDeltas.zip(trialKinematicEvents).sortBy(_._1)
          closestKinematicEvent.headOption match {
            case Some(e) => {
              if (e._1 <= maxAcceptableTimeDifference)
            	Some(kineticEvent, e._2)
              else
                None
            }
            case None    => None
          }
        }
      closestKinematicEvents.filter(_.isDefined).map(_.get)
    } 
    print(" DONE\n")
    
    // Filter out any kinetic events which refer to the same kinematic event
    print("Filtering out kinetic events which refer to the same kinematic event...")
    val uniqueEvents: Seq[(KineticEvent, HoofKinematicEvent)] = {
      val possibleUniqueMatches: Seq[Option[(KineticEvent, HoofKinematicEvent)]] = 
        for ((ke, hke) <- matchEvents) yield {
          val matchesWithoutThisEvent = matchEvents.filter(_ != (ke, hke))
          if (matchesWithoutThisEvent.map(_._2).contains(hke)) None else Some((ke, hke))
        }
      possibleUniqueMatches.filter(_.isDefined).map(_.get)
    }
    print(" DONE\n")
    val nRemovedDueToNonUniqueness = matchEvents.length - uniqueEvents.length
    println(s"$nRemovedDueToNonUniqueness events removed because they referred to the same kinematic event")
    println(s"${uniqueEvents.length} events left")
   
    // For the remaining event matches, find the mean and standard deviation
    print("Computing mean and stdev of the time difference for remaining matches...")
    val tdiff: Seq[Double] = uniqueEvents.map(e => math.abs(e._1.time - e._2.time))
    val mean = tdiff.sum / tdiff.length.toDouble
    val stdev = {
      // sample standard deviation
      val sumsq = tdiff.map(t => math.pow(t - mean, 2.0)).sum
      math.sqrt(sumsq / (tdiff.length.toDouble - 1.0))
    }
    print(" DONE\n")
    println(s"mean time difference      = ${mean} seconds")
    println(s"sample standard deviation = ${stdev} seconds")
    println(s"min time difference       = ${tdiff.min} seconds")
    println(s"max time difference       = ${tdiff.max} seconds")
  }
}
