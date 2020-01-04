stuck at VFree
problem is it needs a double FList
so instead make an n-mapping
storing a list of type level functions to apply to a type via mapping
then, after having this list of args to map into a type arg
(FList stores the types in a list, and the function to map over them)
these n mapping functions seem to nest the types they are mapped onto
to depth n of unbranching application.
but what happens if one of the previous results created a list that could
be mapped into by one of the other functions
the functions that do this would be of the form of partially applied maps
and exist as usual in the list of functions to be applied in the fcontainer
can these be expaneded into fmaps!? if there are sever fmap functions
in a row in the list of functions, can these be made into an fncontainer!?
would doing this make a tree? is that what it is an ftree? only have htree
FList f xs
FNList fs xs
-- use staggered fold for FNList? ie to have the type of the 
-- inputs match the type of the outputs in a chain, 
-- determined by a list of the intermediate type
-- like a container that has a set type at the value
-- for chains of functions!
-- which is a special restriction of HList.
FNList [f1,f2,fmap f3,fmap f4,f5 ..] xs
the it is actually a tree of type on this staggered funciton list?
like if one function returns a list, and the next maps over it...
then this list could be hetrogenous and the mapping could use fcontainer
is the function that apears in the list then a partially applied
fcontainer constructor (partially applied to f) and the output type
as stored in the list of intermediate types - so then not being a tree?

there should be a type family which can take either
a list of length 1 fncontainer or a fcontainer, and either concatinate
or to return and then concatinate to retrun an fncontainer

it should then be possible to apply a function to this list of 
intermediate types in the sequential application of many functions
as per fmaping them into a fcontainer....
can be acted on by a function that can internally concatinate
any sequence of types that can be factored into fncontainers from
subsequent fmap applications. the interspersal of functions that are
not fmap breaks the sequential chain (subsequence) which can be collapesd

but still the fact that there are fcontainers apparing in the list
of an fncontainer, and that these can be collapsed to fncontainer chunks
means that there is some kind of tree structure? no because chunks
still though, the functions mapped onto the leafs can produce
containers which can be mapped over by subsequent functions in the sequence
where this is then like, fmaping fmaps.
these collections that are at the leafs seem like they could be HFree.
where then the depth of fmap (fmap (fmap ...) correponds to 
how far down the HFree the function is being applied. 
this counting of the number of layers to fmap into
seems like a kind of vertical navigation instruction,
where then other navigations could be envisaged,
such as backing out of a deeper container, or selecting only one branch.

because we have hetrogenous lists, instead of applying a function
to every branch recursing to the leafs, it makes sense to specify
only a function on one specific type, of one of the values, 
specified by an Int position in the list of type params to the hcontainer
and a function just acting on this type instead of all the types 
in the container like an fcontainer.
this seems like the navigation of a zipper to a particular location
the headmap and then the returning of the navigation to rewind the zipper
if instead, a pointer were used, then each of these navigations could
be sperated from the headmap functions, also constituting mapping ops
then the list of functions being fmapped in the fncontainer
is also interspersable by navigation instructions, as of when a 
previous function application turns the nodes of the function
being mapped over in the fncontainer (or the alternate headmap vsn!?)
just like how after a container was made then the functions could
include fmaps over this container, which started the HFree discussion.
the point is that hfree sets up a pointer when the containers at the 
leyers are also pointers, such as zippers. so that navigation into 
branches, and among the branches (vertically and horizontally)
gives rise to the compounded navigation instructions of the overall pointer
then, because this adds complexity to the pointer at each nesting
ie, it would stay 2d if it were not for more complex pointers than 
zippers being used for each layer. 
then the type of the specific headmap position can include internal navs
of the type of the container at that layer. 

for instance there could be a use of these delayed application thunks
at the value level, but also corresponding to singleton style types
where these are used to represent the firsctory structure,
so that updates to this can be deffered, but that queiries can be 
run through as if the changes had been applied, where a mixdown
opperation can actually apply the suspended function applications,
which would correspond to real directory modifications that result
from modifications over the representation of this directory structure
as a datatype.


