 type PetriNetName = String
type PetriNetIdentifier = String

trait Marker[N <: PetriNetName]:
  type PetriNetName = N
  val identifier: PetriNetIdentifier

trait Place[N <: PetriNetName]:
  type PetriNetName = N
  val identifier: String
  val markers: Map[PetriNetIdentifier, Marker[N]]
  val markerCount: Integer
  def withMarkers(newMarkerMap: Map[PetriNetIdentifier, Marker[N]]): Place[N]

trait Transition[N <: PetriNetName]:
  type PetriNetName = N
  val placesWeightsBefore: Set[(Place[N], Integer)]
  val placesWeightsAfter: Set[(Place[N], Integer)]
  val priority: Integer
  def tryTransition(places: Map[PetriNetIdentifier, Place[N]]): Map[PetriNetIdentifier, Place[N]] 

given TransitionOrdering[N <: PetriNetName]: Ordering[Transition[N]] = Ordering.by(_.priority)

trait PetriNet[N <: PetriNetName]:
  val places: Map[PetriNetIdentifier, Place[N]]
  val transitions: Set[Transition[N]]
  def withMarkerState(markers: Map[PetriNetIdentifier, Map[PetriNetIdentifier, Marker[N]]]): PetriNet[N] =
    val newPlaces = places.map(
      (id, place) => 
        if (markers.contains(place.identifier)) 
          then (id, place.withMarkers(markers(place.identifier)))
          else (id, place)
    )
    new PetriNet[N]{
      val places = newPlaces
      val transitions = transitions
    }
  def performTransitions()(using o:Ordering[Transition[N]]) = 
    val sortedTransitions = transitions.sorted
    var currPlaces = places
    for (transition <- sortedTransitions) {
      currPlaces = transition.tryTransition(currPlaces)
    }
    new PetriNet[N]{
      val places = currPlaces
      val transitions = transitions
    }
    
class GenericPetriNet[N <: PetriNetName](
  val places: Map[PetriNetIdentifier, Place[N]],
  val transitions: Set[Transition[N]]
) extends PetriNet[N]

object GenericPetriNet:
  def apply[N <: PetriNetName](places: Map[PetriNetIdentifier, Place[N]], transitions: Set[Transition[N]]) = 
    new GenericPetriNet[N](places, transitions)

