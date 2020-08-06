# QLinear

QLinear is type safe library for linear algebra based on `"macro-constructors"`

## Constructors:
 * matrix
    > ```[matrix| a b ; c d |]``` builds matrix __2x2__  `[ [a, b], [c, d] ]`  
    
    Example: 
    ```haskell
    [matrix| 1 2; 3 4 |] !+! [matrix| 2 3; 4 5 |] == [matrix| 3 5; 7 9 |] 
    ```   
    > Also you can't, for example, add two matrix with different size. 
    ```haskell
   [matrix| 1 2; 3 4 |] !+! [matrix| 1 2 3; 4 5 6; 7 8 9 |] -- will not be compiled
    ```

  * vector
    > ```[vector| a b c d |]``` builds matrix __4x1__ `[ [a], [b], [c], [d] ]`  
 * operator
    > ```[operator| (x, y) => (y, x) |]``` builds matrix __2x2__ of operator `[ [0, 1], [1, 0] ]` that swaps coodrinates  
    > ```[operator| (x, y) => (2 * x, y) |]``` builds matrix __2x2__ of operator `[ [2, 0], [0, 1] ]` that doubles `x` coordinate  
   
   Example: 
    ```haskell
    [operator| (x, y) => (3 * y, x / 2) |] !*! [vector| 2 8 |] == [vector| 24 1 |]
    ```
## Syntax:
   * matrix: `exp11 exp12 .. exp1n; exp21 exp22 .. exp2n; ..; expm1 expm2 .. expmn `
   * vector: `exp1 exp2 .. expn`
   * operator: `(var1, var2, .., varn) =>  (exp1, exp2, .., expn)`  
     where 
     * `exp` is `number literal`, `variable` or `any Haskell expression between` `(` and `)`
     * `var` is Haskell variable   
  
Also there are basic operations as determinant, transposition, etc.