# ExBas
A simple BASIC-inspired interpreter written in OCaml and Zig, focused on expression-oriented syntax.

```plaintext
' FizzBuzz Example
let i = 0
for i = 0, 16 step 1 begin
    print(begin
        if i % 15 == 0 then @yield "FizzBuzz"
        elseif i % 3 == 0 then @yield "Fizz"
        elseif i % 5 == 0 then @yield "Buzz"
        else @yield i
    end)
end
```
