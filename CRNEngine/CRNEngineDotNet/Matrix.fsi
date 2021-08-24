// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

// Minimal implementation of the F# Powerpack Matrix that can be cross-compiled to JavaScript.
// Additional methods can be ported as needed to a JavaScript compatible container.

[<Sealed>]
type internal MatrixFS = 

    /// Get the number of rows in a matrix
    member NumRows : int

    /// Get the number of columns in a matrix
    member NumCols : int

    /// Get the item at the given position in a matrix
    member Item : int * int -> float with get,set

    /// Matrix-vector multiplication. 
    //static member ( * ) : Matrix * Vector -> Vector

    static member Multi : MatrixFS * VectorFS -> VectorFS

and
    [<Sealed>]
    internal VectorFS =

        new : float[] -> VectorFS

        member Length : int

        /// Gets the number of rows in a vector
        member NumRows : int

        /// Gets an item from a vector
        member Item : int -> float with get,set

        /// Return a new array containing a copy of the elements of a vector
        member ToArray : unit -> float[]

        member Values : float[]

        (*/// Multiply a scalar value by each element of a vector.
        static member ( * ) : float * Vector -> Vector

        /// Multiply each element of a vector by a scalar value.
        static member ( * ) : Vector * float -> Vector

        /// Add each element of a vector to a float.
        static member ( + ) : float * Vector -> Vector

        /// Add a float to each element of a vector.
        static member ( + ) : Vector * float -> Vector

        /// Add two vectors, pointwise.
        static member ( + ) : Vector * Vector -> Vector

        /// Subtract each element of a vector from a float.
        static member ( - ) : float * Vector -> Vector

        /// Subtract a float from each element of a vector.
        static member ( - ) : Vector * float -> Vector

        /// Subtract two vectors, pointwise.
        static member ( - ) : Vector * Vector -> Vector*)

        /// Multiply a scalar value by each element of a vector.
        static member Multiply : float * VectorFS -> VectorFS

        /// Multiply each element of a vector by a scalar value.
        static member Multiply : VectorFS * float -> VectorFS

        /// Add each element of a vector to a float.
        static member Add : float * VectorFS -> VectorFS

        /// Add a float to each element of a vector.
        static member Add : VectorFS * float -> VectorFS

        /// Add two vectors, pointwise.
        static member Add : VectorFS * VectorFS -> VectorFS

        /// Subtract each element of a vector from a float.
        static member Subtract : float * VectorFS -> VectorFS

        /// Subtract a float from each element of a vector.
        static member Subtract : VectorFS * float -> VectorFS

        /// Subtract two vectors, pointwise.
        static member Subtract : VectorFS * VectorFS -> VectorFS

type internal matrixFS = MatrixFS
type internal vectorFS = VectorFS

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal MatrixFS =

    /// Create a matrix with all entries zero
    //val zero : int -> int -> matrix

    /// Create a matrix with all entries the given constant
    val create : int -> int -> float -> matrixFS

    /// Get an element of a matrix
    //val get : matrix -> int * int -> float

    /// Set an element of a matrix
    //val set : matrix -> int * int -> float -> unit

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal VectorFS =
    
    /// Get an element of a column vector
    val get   : vectorFS -> int -> float

    /// Create a vector from a list of numbers
    val ofList : float list -> vectorFS

    /// Return a new array containing a copy of the elements of the given vector
    val toArray : vectorFS -> float array
    
    /// Generate a vector of the given length where each entry contains the given value
    val create : int -> float -> vectorFS
   
    /// Fold a function over all elements of a vector
    val fold : ('T -> float -> 'T) -> 'T -> vectorFS -> 'T 