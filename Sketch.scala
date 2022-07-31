type PetriNetName = String
type PetriNetIdentifier = String

trait Marker[N <: PetriNetName]:
  type PetriNetName = N
  val identifier: PetriNetIdentifier

trait Place[N <: PetriNetName]:
  type PetriNetName = N
  val identifier: String
  val markers: Map[PetriNetIdentifier, Marker]
  val markerCount: Integer
  def withMarkers(newMarkerMap: Map[PetriNetIdentifier, Marker]: Place

trait Transition[N <: PetriNetName]:
  type PetriNetName = N
  val placesWeightsBefore: Set[(Place, Integer)]
  val placesWeightsAfter: Set[(Place, Integer)]
  val priority: Integer
  def tryTransition(places: Map[PetriNetIdentifier, Place]): Map[PetriNetIdentifier, Place] 

given TransitionOrdering: Ordering[Transition] = Ordering.by(_.priority)

trait PetriNet[N <: PetriNetName]:
  val places: Map[PetriNetIdentifier, Place[N]]
  val transitions: Set[Transition[N]]
  def withMarkerState(markers: Map[PetriNetIdentifier, Map[PetriNetIdentifier, Marker]]): PetriNet =
    val newPlaces = places.map(
      place => 
        if (markers.contains(place.identifier)) 
          then place.withMarkers(markers(place.identifier))
          else place
    )
    new PetriNet[N]{
      val places = newPlaces
      val transitions = transitions
    }
  def performTransitions() = 
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
  val transitions: Set[Transitions[N]]
) extends PetriNet[N]

object GenericPetriNet:
  def apply[N <: PetriNetName](places: Map[PetriNetIdentifier, Place[N]], transitions: Set[Transition[N]]) = 
    new GenericPetriNet[N](places, transitions)
