\name{NEWS}
\title{Glmbb News}
\encoding{UTF-8}

\section{CHANGES IN glmbb version 0.3}{
    \itemize{
        \item Fixed horrible bug discovered by Aaron Rendahl where
        when some variable names were substrings of other variable names
        not all models were searched.

        This could have been avoided if I had remembered the famous
        quote by Jamie Zawinski: \dQuote{Some people, when confronted with
        a problem, think \sQuote{I know, I'll use regular expressions.}
        Now they have two problems.}

        \item Now \code{AICc == Inf} when {n <= p + 1}.

        \item Now error when \code{AIC(out)} is when \code{out} is output
        of R function \code{glm}.
    }
}

\section{CHANGES IN glmbb version 0.2}{
    \itemize{
        \item Added graphical models.  Now can search only over graphical
        models.
    }
}

\section{CHANGES IN glmbb version 0.1}{
    \itemize{
        \item First submission to Github.  Does all hierarchical models.
    }
}

