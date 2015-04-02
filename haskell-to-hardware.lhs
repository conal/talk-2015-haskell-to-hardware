%% -*- latex -*-

%% %let atwork = True

% Presentation
\documentclass{beamer}
%% \documentclass[handout]{beamer}

%% % Printed, 2-up
%% \documentclass[serif,handout]{beamer}
%% \usepackage{pgfpages}
%% \pgfpagesuselayout{2 on 1}[border shrink=1mm]

%% % Printed, 4-up
%% \documentclass[serif,handout,landscape]{beamer}
%% \usepackage{pgfpages}
%% \pgfpagesuselayout{4 on 1}[border shrink=1mm]

\usefonttheme{serif}

\usepackage{hyperref}
\usepackage{color}

\definecolor{linkColor}{rgb}{0.62,0,0}

\hypersetup{colorlinks=true,urlcolor=linkColor}

%% \usepackage{beamerthemesplit}

%% % http://www.latex-community.org/forum/viewtopic.php?f=44&t=16603
%% \makeatletter
%% \def\verbatim{\small\@verbatim \frenchspacing\@vobeyspaces \@xverbatim}
%% \makeatother

\usepackage{graphicx}
\usepackage{color}
\DeclareGraphicsExtensions{.pdf,.png,.jpg}

%% \usepackage{wasysym}
\usepackage{mathabx}
\usepackage{setspace}
\usepackage{enumerate}

\useinnertheme[shadow]{rounded}
% \useoutertheme{default}
\useoutertheme{shadow}
\useoutertheme{infolines}
% Suppress navigation arrows
\setbeamertemplate{navigation symbols}{}

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt

%include mine.fmt

%% %% A subset of mine.fmt. Steve wanted more conventional looking code.
%% %format not = "\Varid{not}"
%% %format :# = "\mathbin{:\!\#}"
%% %format ~> = "\leadsto"
%% %format Prod (k) a b = a "\times_{\hspace{-0.1ex}\scriptscriptstyle{" k "}}" b
%% %format Coprod (k) a b = a "+_{\hspace{-0.4ex}\scriptscriptstyle{" k "}}" b
%% %format Exp (k) a b = a "\Rightarrow_{\hspace{-0.4ex}\scriptscriptstyle{" k "}}" b
%% %format *** = "\!\times\!"
%% %format &&& = "\mathbin{\smalltriangleup}"
%% %format ||| = "\mathbin{\smalltriangledown}"
%% %format +++ = "\!+\!"
%% %format N0 = 0
%% %format N1 = 1
%% %format N2 = 2
%% %format N3 = 3
%% %format N4 = 4
%% %format N5 = 5
%% %format N6 = 6
%% %format N7 = 7
%% %format N8 = 8
%% %% Tweaked
%% %format <$> = "\mathbin{<\!\!\$\!\!>}"
%% %format <.> = "\mathbin{<\!.\!>}"
%% %% hack: add missing space, e.g., before "{" in data type decl
%% %format SPACE = "\ {}"
%% %% Small space
%% %format SSPACE = "{}"
%% %format :=> = "\dashrightarrow"
%% \setbeamerfont{frametitle}{size=\small}

\title{From Haskell to Hardware via CCCs}
\author{\href{http://conal.net}{Conal Elliott}}
\institute{\href{http://tabula.com/}{Tabula}}
% Abbreviate date/venue to fit in infolines space
\date{August, 2014}
% \date{\emph{Draft of \today}}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{1ex}

% \setlength{\blanklineskip}{1.5ex}

\nc\pitem{\pause \item}

%%%%

% \setbeameroption{show notes} % un-comment to see the notes

\setstretch{1.2}

\begin{document}

\frame{\titlepage}

\framet{Tabula}{

\begin{itemize}\parskip0.5ex
\item Founded by Steve Teig about 10 years ago.
\item Post-FPGA reconfigurable hardware.
\item Spacetime architecture:
  \begin{itemize}
  \item 3D for shorter paths
  \item Implemented by rapid reconfiguration (2GHz)
  \item Minkowski spacetime (special relativity)
  \item Spacetime layout with causality constraints
  \item Very high sustained throughput
  \end{itemize}
\item Tremendous flexibility for moving computations in space \& time
\item Program in a non-sequential language: Haskell
\item Compiler \href{https://github.com/conal/lambda-ccc/}{developed openly} and shared freely
\end{itemize}

}

\framet{Example}{

> square :: Num a => a -> a
> square a = a * a

> sumSquare :: (Functor f, Foldable f, Num a) => f a -> a
> sumSquare = sum . fmap square

}

\framet{|sumSquare :: Tree N2 Int -> Int|}{

\begin{center}
\begin{minipage}[c]{0.35\textwidth}\ \end{minipage}
\begin{minipage}[c]{0.4\textwidth}

> sumSquare = sum . fmap square

\end{minipage}
\end{center}

\vspace{-13ex}
\wfig{3.2in}{figures/sumSquare-t2}

}

\framet{|sumSquare :: Tree N2 Int -> Int|}{

\begin{center}
\begin{minipage}[c]{0.0\textwidth}
{\tiny
\begin{verbatim}
module sumSquare-t2 (In_0, In_1, In_2, In_3, Out);
  input [0:31] In_0;
  input [0:31] In_1;
  input [0:31] In_2;
  input [0:31] In_3;
  output [0:31] Out;
  wire [0:31] w_mul_I1;
  wire [0:31] w_mul_I2;
  wire [0:31] w_mul_I3;
  wire [0:31] w_mul_I4;
  wire [0:31] w_add_I5;
  wire [0:31] w_add_I6;
  wire [0:31] w_add_I7;
  assign w_mul_I1 = In_0 * In_0;
  assign w_mul_I2 = In_1 * In_1;
  assign w_mul_I3 = In_2 * In_2;
  assign w_mul_I4 = In_3 * In_3;
  assign w_add_I5 = w_mul_I1 + w_mul_I2;
  assign w_add_I6 = w_mul_I3 + w_mul_I4;
  assign w_add_I7 = w_add_I5 + w_add_I6;
  assign Out = w_add_I7;
endmodule
\end{verbatim}
}
\end{minipage}
\hspace{-7ex}
\begin{minipage}[c]{0.5\textwidth}
\wfig{2.5in}{figures/sumSquare-t2}
\end{minipage}
\end{center}
}

\framet{|sumSquare :: Tree N3 Int -> Int|}{
\vspace{0ex}
\wfig{3in}{figures/sumSquare-t3}
}

\framet{|sumSquare :: Tree N4 Int -> Int|}{
\vspace{0ex}
\wfig{2.9in}{figures/sumSquare-t4}
}

\framet{}{\begin{center} \huge{\emph{\textcolor{blue}{How it works}}} \end{center}}

\framet{Overall plan}{

\begin{itemize}
% \item Use GHC to convert full Haskell to small Core language.
\item Convert Haskell to Core (GHC).
\item Monomorphize.
\item Convert to abstract vocabulary.
\item Interpret as circuits.
%% \item Translate to netlists.
\item Synthesize \& optimize with existing HDL machinery.
\end{itemize}

\vspace{2ex}

Initial simplifications:
\begin{itemize}
\item Shape-typed data
\item Combinational
\end{itemize}

}

\framet{GHC Core}{

> data Expr b	-- ``b'' for the type of binders, 
>   =  Var   Id
>   |  Lit   Literal
>   |  App   (Expr b) (Expr b)
>   |  Lam   b (Expr b)
>   |  Let   (Bind b) (Expr b)
>   |  Case  (Expr b) b Type [Alt b]
>   |  Cast  (Expr b) Coercion
>   |  Type  Type
> 
> type Alt b = (AltCon, [b], Expr b)
> 
> data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT
> 
> data Bind b = NonRec b (Expr b) | Rec [(b, Expr b)]

}

\framet{Overloading lambda}{\parskip3ex

Powerful abstraction mechanisms:
\begin{itemize}\parskip1ex
\item Lambda/application
\item Type classes
\end{itemize}

\vspace{1ex}

Can we use type classes to generalize lambda \& application?

}

\framet{(Bi-)Cartesian closed categories}{

\begin{itemize}\parskip2ex
\item
  \emph{Category}: identity and composition
\item
  \emph{Cartesian}: products
\item
  \emph{Co-Cartesian}: coproducts (``sums'')
\item
  \emph{Closed}: exponentials (arrows as ``values'')
\end{itemize}

\ 

Suffices for translating typed lambda calculus (J. Lambek, 1980).

}

\framet{Category}{

Interface:

> class Category (~>) where
>   id   :: a ~> a
>   (.)  :: (b ~> c) -> (a ~> b) -> (a ~> c)

Laws:

> id . f       == f
> g . id       == g
> (h . g) . f  == h . (g . f)

}

\setlength{\fboxsep}{-1ex}

\framet{Products}{

> class Category (~>) => ProductCat (~>) where
>   type Prod (~>) a b
>   exl      ::  (Prod (~>) a b) ~> a
>   exr      ::  (Prod (~>) a b) ~> b
>   (&&&)    ::  (a ~> c)  -> (a ~> d)  -> (a ~> (Prod (~>) c d))
>   (***)    ::  (a ~> c)  -> (b ~> d)  -> ((Prod (~>) a b) ~> (Prod (~>) c d))
>   f *** g  =   (f . exl ) &&& (g . exr)

Laws:
\vspace{-1.3ex}

\begin{center}
\fbox{\begin{minipage}[c]{0.41\textwidth}

> exl . (f &&& g)      == f                
> exr . (f &&& g)      == g                
> exl . h &&& exr . h  == h                
> exl &&& exr          == id               

\end{minipage}}
\fbox{\begin{minipage}[c]{0.6\textwidth}

> (f *** g) . (h &&& k)  == (f . h) &&& (g . k)     
> id *** id              == id               
> (f *** g) . (h *** k)  == (f . h) *** (g . k)     
> (f &&& g) . h          == (f . h) &&& (g . h)

\end{minipage}}
\end{center}

}

\framet{Coproducts}{

> class Category (~>) => CoproductCat (~>) where
>   type Coprod (~>) a b
>   inl      ::  a ~> (Coprod (~>) a b)
>   inr      ::  b ~> (Coprod (~>) a b)
>   (|||)    ::  (a ~> c)  -> (b ~> c)  -> ((Coprod (~>) a b) ~> c)
>   (+++)    ::  (a ~> c)  -> (b ~> d)  -> ((Coprod (~>) a b) ~> (Coprod (~>) c d))
>   f +++ g  =   (inl . f ) ||| (inr . g)

Laws (dual to product):
\vspace{-1.7ex}

\begin{center}
\fbox{\begin{minipage}[c]{0.41\textwidth}

> (f ||| g) . inl      == f
> (f ||| g) . inr      == g
> h . inl ||| h . inr  == h
> inl ||| inr          == id

\end{minipage}}
\fbox{\begin{minipage}[c]{0.6\textwidth}

> (h ||| k) . (f +++ g)  == (h . f) ||| (k . g)     
> id +++ id              == id               
> (h +++ k) . (f +++ g)  == (h . f) +++ (k . g)     
> h . (f ||| g)          == (h . f) ||| (h . g)

\end{minipage}}
\end{center}
}

\framet{Exponentials}{

> class ProductCat (~>) => ClosedCat (~>) where
>   type Exp (~>) a b
>   apply    :: (Prod (~>) (Exp (~>) a b) a) ~> b
>   curry    :: ((Prod (~>) a b) ~> c) -> (a ~> (Exp (~>) b c))
>   uncurry  :: (a ~> (Exp (~>) b c)) -> (Prod (~>) a b) ~> c

}

\framet{Lambda terms}{

> data E :: * -> * SPACE where
>   Var    :: V a -> E a
>   Const  :: Prim a -> E a
>   App    :: E (a -> b) -> E a -> E b
>   Lam    :: Pat a -> E b -> E (a -> b)

> data Pat :: * -> * SPACE where
>   UnitPat  :: Pat Unit
>   VarPat   :: V a -> Pat a
>   PairPat  :: Pat a -> Pat b -> Pat (a :* b)

}

\framet{Lambda to CCC}{

> (\ p -> k)         :=>  const k
> (\ p -> v)         :=>  ...      -- accessor
> (\ p -> u v)       :=>  apply . ((\ p -> u) &&& (\ p -> v))
> (\ p -> \ q -> u)  :=>  curry (\ (p,q) -> u)

\pause

> convert ::  CCC (~>) => Pat a -> E b -> (a ~> b)
> convert _  (Const x)   = constArrow x
> convert p  (Var v)     = convertVar p v
> convert p  (App u v)   = apply . (convert p u &&& convert p v)
> convert p  (Lam q e)   = curry (convert (PairPat p q) e)

%% \hrefc{https://github.com/ku-fpg/lambda-ccc/blob/master/src/LambdaCCC/ToCCC.hs}{source}

}

\framet{Circuit CCC}{

> data Comp = forall a b. Comp (Prim a b) (Buses a) (Buses b)
>
> type CircuitM = WriterT (Seq Comp) (State BusSupply)

> newtype a :> b = Circ (Buses a -> CircuitM (Buses b))

> instance Category      (:>) where ...
> instance ProductCat    (:>) where ...
> instance ClosedCat     (:>) where ...

}

\framet{}{\begin{center} \huge{\emph{\textcolor{blue}{Examples}}} \end{center}}

\framet{|sumSquare :: Tree N2 Int -> Int|}{

\begin{center}
\begin{minipage}[c]{0.35\textwidth}\ \end{minipage}
\begin{minipage}[c]{0.4\textwidth}

> sumSquare = sum . fmap square

\end{minipage}
\end{center}

\vspace{-13ex}
\wfig{3.2in}{figures/sumSquare-t2}

}

\framet{|sumSquare :: Tree N2 Int -> Int|}{

\begin{center}
{\tiny
\begin{verbatim}

uncurry (apply . (curry (apply . (curry (apply . (curry (apply . (curry (curry
(apply . (curry (apply . (apply . (curry (curry (uncurry add) . exr) . it &&&
apply . (curry (repr . exr) . it &&& apply . (((((id . exr) . exl) . exl) . exl)
. id *** (id . exl) . id))) &&& apply . (curry (repr . exr) . it &&& apply .
(((((id . exr) . exl) . exl) . exl) . id *** (id . exr) . id)))) &&& apply .
(curry (repr . exr) . it &&& apply . (curry (apply . (curry (abst . exr) . it
&&& apply . (apply . (curry (curry id . exr) . it &&& apply . (((id . exr) .
exl) . id *** (id . exl) . id)) &&& apply . (((id . exr) . exl) . id *** (id .
exr) . id)))) &&& apply . (curry (repr . exr) . it &&& apply . (curry (repr .
exr) . it &&& id . exr))))))) &&& curry (apply . (curry (abst . exr) . it &&&
apply . (curry (apply . (curry (abst . exr) . it &&& apply . (apply . (curry
(curry id . exr) . it &&& apply . (((id . exr) . exl) . id *** (id . exl) . id))
&&& apply . (((id . exr) . exl) . id *** (id . exr) . id)))) &&& apply . (curry
(repr . exr) . it &&& apply . (curry (repr . exr) . it &&& id . exr))))))) &&&
curry (apply . (curry (apply . (curry (abst . exr) . it &&& apply . (apply .
(curry (curry (uncurry mul) . exr) . it &&& id . exr) &&& id . exr))) &&& apply
. (curry (repr . exr) . it &&& id . exr))))) &&& curry (apply . (curry (apply .
(curry (abst . exr) . it &&& apply . (apply . (curry (curry (uncurry add) . exr)
. it &&& apply . (curry (repr . exr) . it &&& apply . (((id . exr) . exl) . id
*** (id . exl) . id))) &&& apply . (curry (repr . exr) . it &&& apply . (((id .
exr) . exl) . id *** (id . exr) . id))))) &&& apply . (curry (repr . exr) . it
&&& apply . (curry (repr . exr) . it &&& id . exr)))))) &&& curry (apply .
(curry (abst . exr) . it &&& apply . (curry (repr . exr) . it &&& id . exr)))))
. (it &&& id)

\end{verbatim}
}
\end{center}
}

\framet{|\ (a,b) -> a+b :: Int|}{\parskip3ex

\begin{center}
\begin{minipage}[c]{0.45\textwidth}
\wfig{2.3in}{figures/sum-2}
\end{minipage}
\begin{minipage}[c]{1\textwidth}

{\small
\begin{verbatim}
module sum-2 (In_0, In_1, Out);
  input [0:31] In_0;
  input [0:31] In_1;
  output [0:31] Out;
  wire [0:31] w_add_I1;
  assign w_add_I1 = In_0 + In_1;
  assign Out = w_add_I1;
endmodule
\end{verbatim}
}
\end{minipage}
\end{center}
}

\framet{|\ (a,b,c,d) -> a+b+c+d :: Int|}{
\vspace{-1ex}
\wfig{3.45in}{figures/sum-4a}
}

\framet{|\ (a,b,c,d) -> (a+b)+(c+d) :: Int|}{
%\vspace{-2ex}
\wfig{3.75in}{figures/sum-4b}
}

\framet{Uniform pairs}{

\vspace{8ex}

> data Pair a = a :# a

\vspace{8ex}

|Functor|, |Applicative|, |Monad|, |Foldable|, |Traversable|.

}

\framet{|sum :: Pair Int -> Int|}{
\wfig{3.5in}{figures/sum-p}
}

\framet{|sumSquare :: Pair Int -> Int|}{
%\vspace{-2ex}
\wfig{3.7in}{figures/sumSquare-p}
}

\framet{Length-typed vectors}{

> data VecTy :: Nat -> * -> * SPACE where
>   ZVec  :: Vec Z a 
>   (:<)  :: a -> Vec n a -> Vec (S n) a

\pause
\vspace{-5ex}

> instance Functor (Vec n) where
>   fmap _ ZVec      = ZVec
>   fmap f (a :< u)  = f a :< fmap f u
>
> instance Foldable (Vec n) where
>   foldMap _ ZVec       = mempty
>   foldMap h (a :< as)  = h a <> foldMap h as

And |Applicative|, |Monad|, |Traversable|.

}

\framet{|fmap not :: Vec N6 Bool -> Vec N6 Bool|}{
\vspace{-1.8ex}
\wfig{3in}{figures/map-v6}
}

\framet{|sum :: Vec N6 Int -> Int|}{
\vspace{-1ex}
\wfig{3.2in}{figures/sum-v6-0}
}

\framet{Improved folding}{

Bypass |mempty| when possible.

> instance Foldable (Vec n) where
>   foldMap _ ZVec         = mempty
>   foldMap h as@(_ :< _)  = foldMapS h as

> foldMapS :: Monoid m => (a -> m) -> Vec (S n) a -> m
> foldMapS h (a :< as) =
>   case as of
>     ZVec      -> h a
>     (_ :< _)  -> h a <> foldMapS h as

}

\framet{|sum :: Vec N6 Int -> Int|}{
\vspace{-1ex}
\wfig{3.3in}{figures/sum-v6}
}

\framet{Depth-typed trees}{

> data TreeTy :: Nat -> * -> * SPACE where
>   L  :: a -> Tree Z a
>   B  :: Pair (Tree n a) -> Tree (S n) a

\pause
\vspace{-5ex}

> instance Functor (Tree n) where
>   fmap f (L a   ) = L (f a)
>   fmap f (B ts  ) = B ((fmap.fmap) f ts)
>
> instance Foldable (Tree n) where
>   foldMap f (L a   ) = f a
>   foldMap f (B ts  ) = (foldMap.foldMap) f ts

And |Applicative|, |Monad|, |Traversable|.

Easily generalize beyond |Pair|.

}

\framet{|fmap not :: Tree N3 Bool -> Tree N3 Bool|}{
\vspace{-1ex}
\wfig{2.83in}{figures/map-t3}
}

\framet{|sum :: Tree N4 Int -> Int|}{
\vspace{-1ex}
\wfig{3.1in}{figures/sum-t4}
}

\framet{|sum :: Tree N3 Int -> Int|}{

\vspace{-1ex}
Monomorphized \& simplified GHC Core:

{\tiny

> let  f0 :: Tree N0 Int -> Sum Int
>      f0 = \ ds ->
>        abst ZfRepSum (repr ZfRepTree0 ds)
>      f1 :: Tree N1 Int -> Sum Int
>      f1 = \ ds ->
>        case repr ZfRepPair (repr ZfRepTree ds) of
>          (,) a b ->
>            abst ZfRepSum
>                 (ZfNumInt_ZcP  (repr ZfRepSum (f0 a))
>                                (repr ZfRepSum (f0 b)))
>      f2 :: Tree N2 Int -> Sum Int
>      f2 = \ ds ->
>        case repr ZfRepPair (repr ZfRepTree ds) of
>          (,) a b ->
>            abst ZfRepSum
>                 (ZfNumInt_ZcP  (repr ZfRepSum (f1 a))
>                                (repr ZfRepSum (f1 b)))
> in SPACE \ eta ->
>       repr ZfRepSum
>            (case repr ZfRepPair (repr ZfRepTree eta) of
>               (,) a b ->
>                 abst ZfRepSum
>                      (ZfNumInt_ZcP  (repr ZfRepSum (f2 a))
>                                     (repr ZfRepSum (f2 b))))

}
}

\framet{Dot products}{

> dot ::  (Foldable g, Foldable f, Num (f a), Num a) =>
>         g (f a) -> a
> dot = sum . product

Typically, |g == Pair|.
\pause % \vspace{2ex}

Alternatively,

> dot ::  (Traversable g, Foldable f, Applicative f, Num a) =>
>         g (f a) -> a
> dot = sum . fmap product . transpose

where

> transpose :: (Traversable t, Applicative f) => t (f a) -> f (t a)

}

\framet{|transpose :: Pair (Tree N4 Int) -> Tree N4 (Pair Int)|}{
\vspace{-2ex}
\wfig{1.83in}{figures/transpose-pt4}
}

\framet{|transpose :: Tree N4 (Pair Int) -> Pair (Tree N4 Int)|}{
\vspace{-2ex}
\wfig{1.83in}{figures/transpose-t4p}
}

\framet{|dot :: Pair (Tree N1 Int) -> Int|}{
\vspace{-2ex}
\wfig{4.1in}{figures/dotsp-pt1}
}

\framet{|dot :: Pair (Tree N2 Int) -> Int|}{
\vspace{-2ex}
\wfig{3.25in}{figures/dotsp-pt2}
}

\framet{|dot :: Pair (Tree N3 Int) -> Int|}{
\vspace{-3ex}
\wfig{3.25in}{figures/dotsp-pt3}
}

\framet{|dot :: Pair (Tree N4 Int) -> Int|}{
\vspace{-2ex}
\wfig{3in}{figures/dotsp-pt4}
}

\framet{|dot :: Vec N3 (Tree N2 Int) -> Int|}{
\vspace{-3ex}
\wfig{3.3in}{figures/dotsp-v3t2}
}

\framet{|dot :: Tree N2 (Tree N2 Int) -> Int|}{
\vspace{-3ex}
\wfig{3.3in}{figures/dotsp-t2t2}
}

\framet{Linear transformations}{

> (<.>) :: (Foldable f, Num (f a), Num a) => f a -> f a -> a
> u <.> v = dot (u :# v)

\pause

> type Matrix m n a = Vec n (Vec m a)

> ($@) :: (IsNat m, Num a) => Matrix m n a -> Vec m a -> Vec n a
> mat $@ vec = (<.> vec) <$> mat

}

\framet{|($@) :: Matrix N2 N3 Int -> Vec N2 Int -> Vec N3 Int|}{
\vspace{-1ex}
\wfig{3in}{figures/applyLin-v23}
}


% main = go "applyLin-v42" (uncurry (($@) :: Matrix N4 N2 Int -> Vec N4 Int -> Vec N2 Int))

\framet{|($@) :: Matrix N4 N2 Int -> Vec N4 Int -> Vec N2 Int|}{
\vspace{-1ex}
\wfig{2.95in}{figures/applyLin-v42}
}

\framet{|Generalizing linear transformations|}{

> ($@) ::  (IsNat m, Num a) =>
>          Vec n (Vec m a) -> Vec m a -> Vec n a
> mat $@ vec = (<.> vec) <$> mat

More simply and generally,

> ($@) ::  (Foldable m, Applicative m, Functor n, Num a) =>
>          n (m a) -> m a -> n a

For instance,

> type MatrixT m n a = Tree n (Tree m a)

}

\framet{|($@) :: MatrixT N2 N1 Int -> Tree N2 Int -> Tree N1 Int|}{
\vspace{-1ex}
\wfig{2.75in}{figures/applyLin-t21}
}

\framet{|($@) :: MatrixT N2 N2 Int -> Tree N2 Int -> Tree N2 Int|}{
\vspace{-1ex}
\wfig{2.75in}{figures/applyLin-t22}
}

\framet{Composing linear transformations}{

Transform columns:

> (.@) ::  (IsNat m, IsNat n, IsNat o, Num a) =>
>          Matrix n o a -> Matrix m n a -> Matrix m o a
> no .@ mn = transpose ((no SSPACE $@) <$> transpose mn)

More simply and generally,

> (.@) ::  ( Applicative o, Traversable n, Applicative n
>          , Traversable m, Applicative m, Num a ) =>
>          o (n a) -> n (m a) -> o (m a)

}

\framet{|(.@) :: Matrix N3 N4 Int -> Matrix N2 N3 Int -> Matrix N2 N4 Int|}{
\vspace{-1ex}
\wfig{2.6in}{figures/composeLin-v234}
}

\framet{|(.@) :: MatrixT N2 N2 Int -> MatrixT N2 N2 Int -> MatrixT N2 N2 Int|}{
\vspace{-1ex}
\wfig{2.6in}{figures/composeLin-t222}
}

\framet{|(.@) :: MatrixT N3 N2 Int -> MatrixT N2 N3 Int -> MatrixT N2 N2 Int|}{
\vspace{-1ex}
\wfig{2.7in}{figures/composeLin-t232}
}

\framet{Status and future}{

\begin{itemize}\parskip1ex
\item \href{https://github.com/conal/lambda-ccc/}{GitHub repository}
\item Looking for collaboration and hiring recommendations
\item To do:
  \begin{itemize}\parskip1ex
  \item Improve performance
  \item More examples
  \item Genuine sums for circuits
  \item Memory and computation management
  \item More interpretations (CCCs)
  \end{itemize}
\end{itemize}
}

\end{document}
