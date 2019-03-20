---
documentclass: article
title: Design of the lambda cube interpreter
author: Thomas Tan
date: 18 March 2019
papersize: a4
# For option documentation, see https://github.com/jez/latex-homework-class
classoption:
- 11pt
- largemargins
# Remove or change this line if you don't have the Menlo font installed
newtxmathoptions:
- cmintegrals
- cmbraces
colorlinks: true
linkcolor: RoyalBlue
urlcolor: RoyalBlue
header-includes:
- \usepackage{mathpartir}
- \usepackage{amsmath}
- \usepackage[document]{ragged2e}
- \setlength{\parindent}{1em}
# Commands
- \newcommand{\eval}{\ensuremath{\Downarrow}}

- \newcommand{\val}{\ensuremath{\text{ val}}}
- \newcommand{\type}{\ensuremath{\text{ type}}}
- \renewcommand{\and}{\ensuremath{\text{ and }}}
- \newcommand{\when}{\ensuremath{\text{ when }}}
- \newcommand{\true}{\ensuremath{\mathit{true}}}
- \newcommand{\false}{\ensuremath{\mathit{false}}}
- \newcommand{\Bool}{\ensuremath{\mathit{Bool}}}
- \newcommand{\Nat}{\ensuremath{\mathit{Nat}}}
- \newcommand{\Seq}{\ensuremath{\mathit{Seq}}}
- \newcommand{\iftt}{\ensuremath{\mathtt{if}}}
- \newcommand{\aptt}{\ensuremath{\mathtt{ap}}}

- \newcommand{\vect}[2]{#1_1,#1_2,\dots,#1_{#2}}
- \newcommand{\arr}[2]{\langle \vect{#1}{#2} \rangle}
- \newcommand{\tup}[1]{\langle#1\rangle}
...

\newcommand{\ft}{\mathit{ft}\ }
\newcommand{\ftf}[2]{(\mathit{ft}\ #1\ #2)}
\newcommand{\ftProp}{\mathit{ft}*\ }
\newcommand{\ftVar}{\mathit{ftVar}\ }
\newcommand{\ftLambda}{\mathit{ft}\lambda\ }
\newcommand{\ftPi}{\mathit{ft}\Pi\ }
\newcommand{\ftApp}{\mathit{ftApp}\ }

\newcommand{\GammaL}{\Gamma_L}
\newcommand{\GammaR}{\Gamma_R}

\maketitle

# Basic syntax and object representation

- Prop ($*$, denoted by `*` or `Prop` in language, `CocSyntaxProp`)
- Type ($\square$, denoted by `@` or `Type` in language, `CocSyntaxType`)
- variables (alphanumeric names, `CocSyntaxVariable label`)
- application (`(a b)` where a and b are terms, `CocSyntaxApply function argument`)
- lambda abstraction (`(\a:B.c)` where a is a variable, B is the type, and c is the body, `CocSyntaxLambda param inType body`)
- forall abstraction aka product type (`{\a:B.c}` where a is a variable, B is the type, and c is the body, `CocSyntaxForall param inType body`)

We should first convert this to an internal representation using de Brujin indices, but we keep the labels around for fun (readability):

- "Objects" (`CocObject`)
	- Prop (`CocProp`)
	- Type (`CocType`)
	- variables (`CocVariable label index` index being the deBrujin index, 0 if it's used as the param in an abstraction)
	- application (`CocApply function argument`)
	- lambda abstraction (`CocLambda param inType body`)
	- forall abstraction aka product type (`CocForall param inType body`)
- With typing judgements
	- `CocValue { cocObject :: CocObject, cocType :: Maybe CocObject }`

<!-- I originally wanted to perform reductions in normal order at first. -->
<!-- But I realised that actually CoC is strongly normalising. -->
<!-- So we shall go with an applicative order instead which makes things easier in general. -->

<!-- A term is in weak head normal form (WHNF) if its outermost constructor is not a `CocApply`. -->
<!-- We denote that a term $e$ is in WHNF by $N(e)$. -->
<!-- There may be unperformed beta reductions inside the term, but we do not care. -->

# Theory

## Reduction rules

Firstly we write the reduction rules:

<!-- \inferrule{C \mapsto C'}{(\lambda x:A.B) C \mapsto (\lambda x:A.B) C'} \and -->
\begin{mathpar}
	\inferrule*[Right=BetaReduce]
		{ }
		{(\lambda x:A.B) C \mapsto B[C/x]} \\
	\inferrule*[Right=LambdaBodyReduce]
		{B \mapsto B'}
		{(\lambda x:A.B) \mapsto (\lambda x:A.B')} \\
	\inferrule*[Right=LambdaTypeReduce]
		{A \mapsto A'}
		{(\lambda x:A.B) \mapsto (\lambda x:A'.B)} \\
	\inferrule*[Right=ForallBodyReduce]
		{B \mapsto B'}
		{(\Pi x:A.B) \mapsto (\Pi x:A.B')} \\
	\inferrule*[Right=ForallTypeReduce]
		{A \mapsto A'}
		{(\Pi x:A.B) \mapsto (\Pi x:A'.B)}
\end{mathpar}

## Construction rules

And now the typing judgements. These rules double as rules to build up a set of (valid) terms in the language. Here $s_1$ and $s_2$ represent sorts which are either $*$ or $\square$ as mentioned before.
Calculus of constructions allow all combinations of $s_1$ and $s_2$ but weaker systems are restricted.

\begin{gather*}
	\inferrule*[Right=$*C$]
		{ }
		{\vdash * : \square} \\
	\inferrule*[Right=$CtxC$]
		{ }
		{\vdash *} \\
	\inferrule*[Right=$\lambda C$]
		{\Gamma \vdash A : s_1 \\ \Gamma, x:A \vdash b : B \\ \Gamma, x:A \vdash B : s_2}
		{\Gamma \vdash (\lambda x:A.b) : (\Pi x:A.B)} \\
	\inferrule*[Right=$\Pi C$]
		{\Gamma \vdash A : s_1 \\ \Gamma, x:A \vdash B : s_2}
		{\Gamma \vdash (\Pi x:A.B) : s_2} \\
	\inferrule*[Right=$AppC$]
		{\Gamma \vdash C : (\Pi x:A.B) \\ \Gamma \vdash D : A}
		{\Gamma \vdash C\ D : B[D/x]} \\
	\inferrule*[Right=$VarC$]
		{\Gamma_L, x:A, \Gamma_R \vdash *}
		{\Gamma_L, x:A, \Gamma_R \vdash x:A} \\
	\inferrule*[Right=$Weak$]
		{\Gamma \vdash A:s \\ \Gamma \vdash B:C}
		{\Gamma, x:A \vdash B:C} \\
\end{gather*}

The $\varnothing C$ rule may seem out of place but it's purpose is to allow the construction of variables via $VarC$.
In the original paper, it was used to identify which contexts were "valid".
It used the following rules as well

\begin{mathpar}
	\inferrule*[Right=$CtxExt1$]
		{\Gamma \vdash \Delta}
		{\Gamma, x : \Delta \vdash *} \\
	\inferrule*[Right=$CtxExt2$]
		{\Gamma \vdash A : s}
		{\Gamma, x : A \vdash *} \\
\end{mathpar}

which builds up valid contexts. But these rules can be replicated with the $Weak$ rule, I believe.

The $Weak$ rule is necessary to allow construction of sub-expressions which do not use every variable in the context,
and it's obvious to see why it's necessary for even tiny expressions.

## Examples

Here's a derivation for a simple $\lambda$ term.

\begin{mathpar}
	\inferrule*[Right=$\Pi C$]
	{
		\inferrule*[right=$*C$]
			{ }
			{\vdash * : \square} \\
		\inferrule*[Right=$Weak$]
			{
				\inferrule*[Right=$*C$]{ }{\vdash * : \square} \\
				\inferrule*[Right=$*C$]{ }{\vdash * : \square}
			}
			{x : * \vdash * : \square}
	}
	{\vdash (\Pi x:*.*) : \square}
\end{mathpar}

Here's a derivation for a $\Pi$ term.

\begin{mathpar}
	\inferrule*[Right=$\lambda C$]
	{
		\inferrule*[right=$*C$]
		{ }
		{\vdash * : \square} \\
		\inferrule*[right=$VarC$]
		{
			\inferrule*[Right=$CtxExt2$]
			{
				\inferrule*[Right=$*C$]
				{ }
				{\vdash * : \square}
			}
			{x : * \vdash *}
		}
		{x : * \vdash x : *} \\
		\inferrule*[Right=$Weak$]
		{
			\inferrule*[Right=$*C$]{ }{\vdash * : \square} \\
			\inferrule*[Right=$*C$]{ }{\vdash * : \square}
		}
		{x : * \vdash * : \square}
	}
	{\vdash (\lambda x:*.x) : (\Pi x:*.*)}
\end{mathpar}

For apply terms we run into a small hurdle...
Notice that there are unknowns in the precedents that have to be solved.

\begin{gather*}
	\inferrule*[Right=$AppC$]
	{
		\inferrule*
		{ }
		{\vdash (\lambda x:*.x) : (\Pi x:??.*)} \\
		\inferrule*
		{ }
		{\vdash (\Pi y:*.*) : ??}
	}
	{\vdash ((\lambda x:*.x)\ (\Pi y:*.*)) : *[(\Pi y:*.*)/x]}
\end{gather*}

However, it is plain to see that if the function term is known, as it is here $(\lambda x:*.x)$,
then we can see that because the only way to derive a $\lambda$ term is through $\lambda C$,
we know that the ?? must be equal to the type of the lambda's parameter, namely $*$.

We can write a slightly modified $NAppC$ rule that takes handles application where the function term starts with a lambda constructor.

\begin{gather*}
	\inferrule*[Right=$NAppC$]
	{
		\vdash (\lambda x:A.b) : (\Pi x:A.B) \\
		\vdash D : A
	}
	{\vdash ((\lambda x:A.b)\ D) : B[D/x]}
\end{gather*}

Now we can do the original derivation:

\begin{gather*}
	\inferrule*[Right=$NAppC$]
	{
		\inferrule*[Right=$\lambda C$]
		{\vdots}
		{\vdash (\lambda x:*.x) : (\Pi x:*.*)} \\
		\inferrule*[Right=$\Pi C$]
		{\vdots}
		{\vdash (\Pi y:*.y) : *}
	}
	{\vdash ((\lambda x:*.x)\ (\Pi y:*.y)) : *[(\Pi y:*.y)/x]}
\end{gather*}

We can see that naively we already need some basic form of type inference just to determine if terms are derivable.

More issues arise when for example, we only write that
$$ \vdash ((\lambda x:*.x)\ (\Pi y:*.y)) : * $$
and not the pre-substitution form written above.

The result is still true, but how do we run the rules backwards to determine its validity?

This motivates the introduction of conversion rules
which bake the idea of equality containing beta-reduction into the type system.

The equality rules take up a lot of space so I won't bother showing it here.
Instead let's just do a quick derivation for the above problematic judgement.

\begin{mathpar}
	\inferrule*[right=$\beta\cong$]
	{
		\inferrule{\vdots}{\vdash (\Pi y:*.y) : *} \\
		\inferrule{\inferrule{\vdots}{\vdash ((\lambda x:*.x)\ (\Pi y:*.y)) \cong x[(\Pi y:*.y)/x] \\ \vdash x[(\Pi y:*.y)/x] \cong (\Pi y:*.y)}}{\vdash ((\lambda x:*.x)\ (\Pi y:*.y)) \cong (\Pi y:*.y)}
	}
	{
		\vdash ((\lambda x:*.x)\ (\Pi y:*.y)) : *
	}
\end{mathpar}

However just because they are now derivable judgementally doesn't mean it's decidable or efficient.
The original paper says that there exists an efficient (polytime) algorithm to determine if a term is well typed,
but doesn't describe it.

I haven't actually found any article that describes the algorithm proper,
so I've written it down below.

## Find type

We'll define a function $(\ft \Gamma A) = B$ that takes in
a context $\Gamma$ and an expression $A$,
and its result will be its type $B$.

The hope is that the following rule is admissible,

\begin{mathpar}
	\inferrule
	{(\ft \Gamma\ A) = B}
	{\Gamma \vdash A:B}
\end{mathpar}

which would let us use ft as a subroutine or preprocessing step to check that terms are well typed.

\begin{mathpar}
	\inferrule*[Right=$\ftProp$]{\Gamma \vdash *}{\ftf{\Gamma}{*} = \square} \\
	\inferrule*[Right=$\ftVar$]{\Gamma_L, x:A, \Gamma_R \vdash *}{\ftf{\Gamma_L, x:A, \Gamma_R}{x} = A} \\
	\inferrule*[Right=$\ftLambda$]{\ftf{\Gamma}{A} = s_1 \\ \ftf{\Gamma,x:A}{b} = B \\ \ftf{\Gamma,x:A}{B} = s_2}{\ftf{\Gamma}{(\lambda x:A.b)} = (\Pi x:A.B)} \\
	\inferrule*[Right=$\ftPi$]{\ftf{\Gamma}{A} = s_1 \\ \ftf{\Gamma,x:A}{B} = s_2}{\ftf{\Gamma}{(\Pi x:A.B)} = s_2} \\
	\inferrule*[Right=$\ftApp$]{\ftf{\Gamma}{C} = (\Pi x:A.B) \\ \ftf{\Gamma}{a} = A' \\ A = A'}{\ftf{\Gamma}{(C\ a)} = B}
\end{mathpar}

Here's an trace of the algorithm on a simple term.
I'll denote in superscripts a step number which relates where the value comes from.

\begin{mathpar}
	\footnotesize
	\inferrule
	{
		\inferrule
		{
			\inferrule
			{ }
			{\ftf{\varnothing}{*} = \square} \\
			\inferrule
			{x:* \vdash *^1}
			{\ftf{x:*}{x} = *^1}
		}
		{\ftf{\varnothing}{(\lambda x:*.x)} = (\Pi x:*^2.*^1)} \\
		\inferrule
		{
			\inferrule
			{ }
			{\ftf{\varnothing}{*} = \square} \\
			\inferrule
			{x:* \vdash *}
			{\ftf{x:*}{x} = *^3} \\
		}
		{\ftf{\varnothing}{(\Pi x:*.x)} = *^3} \\
		*^2 = *^3
	}
	{\ftf{\varnothing}{((\lambda x:*.x)\ (\Pi x:*.x))} = *^1}
\end{mathpar}
