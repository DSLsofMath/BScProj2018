> module Body.Body where

\begin{code}
import Vector.Vector
\end{code}

In classical mechanics a physical body is a collection of matter having
properties, including mass, velocity, momentum, and energy. The matter exisits
in a volume of three-dimensional space.

\begin{code}
data Body = Body {
  mass :: Double,
  velocity :: Vector3,
  momentum :: Vector3,
  energy :: Double
  }
\end{code}

