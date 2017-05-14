# talk-typeclasses

[00] Instances and Dictionaries
     https://www.schoolofhaskell.com/user/jfischoff/instances-and-dictionaries
[01] OOP vs type classes
     https://wiki.haskell.org/OOP_vs_type_classes
[02] Desugared Dictionaries
     https://ghc.haskell.org/trac/ghc/wiki/DesugaredDictionaries
[03] Making dictionary passing explicit in Haskell
     https://www.joachim-breitner.de/blog/398-Making_dictionary_passing_explicit_in_Haskell
[04] Type classes
     https://en.wikipedia.org/wiki/Type_class
[05] Demystifying typeclasses
     http://okmij.org/ftp/Computation/typeclass.html
[06] Scrap your typeclasses
     http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
[07] INLINE Pragma in combination with type classes
     http://stackoverflow.com/questions/9915271/inline-pragma-in-combination-with-type-classes
[08] C++ templates/traits versus Haskell type classes
     http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.78.2151&rep=rep1&type=pdf
[09] What I wish I knew when learning Haskell
     http://dev.stephendiehl.com/hask/#type-classes
[10] Solution to Multi-Parameter Type class dilemma
     http://homepages.dcc.ufmg.br/~camarao/CT/solution-to-mptc-dilemma.pdf
[11] How to desugar haskell Code
     http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html
[12] Standard Haskell Typeclasses
     https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/stdclasses.html
[13] Haskell Cheatsheet
     http://blog.codeslower.com/static/CheatSheet.pdf
[14] Collection of Typeclass and constraint tricks
     https://www.reddit.com/r/haskell/comments/4yl9je/collection_of_type_class_and_constraint_tricks/
[15] Using Typeclasses
     http://book.realworldhaskell.org/read/using-typeclasses.html
[16] 24 Days of GHC Extensions: Multi-parameter Type Classes
     https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html
[17] Types and typeclasses
     http://learnyouahaskell.com/types-and-typeclasses
[18] Haskell's Type Classes: We Can Do Better
     http://degoes.net/articles/principled-typeclasses
[19] Boston Haskell: Edward Kmett - Type Classes vs. the World
     https://www.reddit.com/r/haskell/comments/2w4ctt/boston_haskell_edward_kmett_type_classes_vs_the/
     https://barnacles.blackfriday/s/zmxxdu/edward_kmett_type_classes_vs_the_world
     https://www.meetup.com/Boston-Haskell/events/219074467/
[20] Java's Interface and Haskell's type class: differences and similarities?
     http://stackoverflow.com/questions/6948166/javas-interface-and-haskells-type-class-differences-and-similarities
[21] What makes type classes better than traits?
     https://www.reddit.com/r/scala/comments/3bh5g8/what_makes_type_classes_better_than_traits/
[22] Type-classes are nothing like interfaces
     http://blog.tmorris.net/posts/type-classes-are-nothing-like-interfaces/
[23] Interfaces vs. Type Classes
     http://lambda-the-ultimate.org/node/4867
[24] Demystifying Implicits and Typeclasses in Scala
     http://www.cakesolutions.net/teamblogs/demystifying-implicits-and-typeclasses-in-scala
[25] Ad Hoc Polymorphism in Scala With Type Classes
     http://blog.jaceklaskowski.pl/2015/05/15/ad-hoc-polymorphism-in-scala-with-type-classes.html
[26] Why I prefer typeclass-based libraries
     http://www.yesodweb.com/blog/2016/03/why-i-prefer-typeclass-based-libraries
[27] Understanding C++ Concepts through Haskell Type Classes
     https://bartoszmilewski.com/2010/11/29/understanding-c-concepts-through-haskell-type-classes/
[28] Overloading and type classes in Haskell
     http://www.cse.chalmers.se/edu/year/2016/course/TDA452/lectures/OverloadingAndTypeClasses.html
[29] Monkey Patching, Duck Typing and Type Classes
     http://jsuereth.com/2010/07/13/monkey-patching-scala.html
[30] Typeclasses: Polymorphism in Haskell
     http://andrew.gibiansky.com/blog/haskell/haskell-typeclasses/
[31] Type classes: confluence, coherence and global uniqueness
     http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/
[32] First-Class Type Classes
     https://www.irif.fr/~sozeau/research/publications/First-Class_Type_Classes.pdf
[33] Type classes (Philip Wadler)
     http://homepages.inf.ed.ac.uk/wadler/topics/type-classes.html
[34] Gang of Four patterns in Haskell
     https://gist.github.com/drostie/818c54ca5c8182143699a72da98618bc
[35] Gang of Four Patterns With Type-Classes and Implicits in Scala
     https://staticallytyped.wordpress.com/2013/03/09/gang-of-four-patterns-with-type-classes-and-implicits-in-scala/
     https://staticallytyped.wordpress.com/2013/03/24/gang-of-four-patterns-with-type-classes-and-implicits-in-scala-part-2/
[36] Typed final (tagless-final) style
     http://okmij.org/ftp/tagless-final/
[37] mono-traversable
     https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable#readme
[38] 5 Ways to Test Applications that Access a Database in Haskell
     https://functor.tokyo/blog/2015-11-20-testing-db-access
[39] Reifying Type Classes with GADTs
     http://lambdalog.seanseefried.com/posts/2011-05-16-reifying-type-classes-with-gadts.html
[40] Type Classes and Overloading
     https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/classes.html
[41] GHC Core by example
     http://alpmestan.com/posts/2013-06-27-ghc-core-by-example-episode-1.html

================================================================================

[19] Joe Fasel "Overloading should be reflected in the type of the function"

[19] Type classes let you add behaviour to an existing data type:

      class Eq a where
        (==) :: a -> a -> Bool

      instance Eq () where
        () == () = True

      instance Eq Bool where
        True  == True  = True
        False == False = True
        _     == _     = False

      instance Eq Int where
        ### TODO ###

      instance Eq Char where
        ### TODO ###

      instance (Eq a, Eq b) => Eq (a, b) where
        ### TODO ###

      Rules, logic programming style of dispatch
      Pattern match our way with the type
      The types change the meaning of the program
      The v-table gets plumbed around by the types

[19] Subclassing:

      class Eq a => Ord a where
        compare :: a -> a -> Ordering

      instance Ord () where
        compare () () = EQ

      instance Ord Bool where
        compare False False = EQ
        compare False True  = LT
        compare True  False = GT
        compare True  True  = EQ

[19] Multiple instances:

      class Semigroup a where
        mappend :: a -> a -> a

      class Semigroup a => Monoid a where
        mempty :: a

      instance Semigroup String where
        mappend = (++)

      instance Monoid String where
        mempty = ""

      instance Semigroup Int where
        mappend = (+)

      instance Monoid Int where
        mempty = 0

      instance Semigroup Int where
        mappend = (*)

      instance Monoid Int where
        mempty = 1

      instance Semigroup Int where
        mappend = max

      instance Monoid Int where
        mempty = minBound

      instance Semigroup Int where
        mappend = min

      instance Monoid Int where
        mempty = maxBound

      newtype Sum a = a deriving (Eq, Show, Num, Bound)

      instance Semigroup (Sum Int) where
        mappend = (+)

      instance Monoid (Sum Int) where
        mempty = 0

      newtype Sum a = a deriving (Eq, Show, Num, Bound)

      instance Semigroup (Product Int) where
        mappend = (*)

      instance Monoid (Product Int) where
        mempty = 1

      newtype Sum a = a deriving (Eq, Show, Num, Bound)

      instance Semigroup (Max Int) where
        mappend = max

      instance Monoid (Max Int) where
        mempty = minBound

      newtype Sum a = a deriving (Eq, Show, Num, Bound)

      instance Semigroup (Min Int) where
        mappend = min

      instance Monoid (Min Int) where
        mempty = maxBound
