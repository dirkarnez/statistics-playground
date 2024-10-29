/// <reference lib="DOM" />

function foo(bar: string) {
    return "Hello " + bar;
}

let baz = "World!";


document.getElementById("root").innerHTML = foo(baz);