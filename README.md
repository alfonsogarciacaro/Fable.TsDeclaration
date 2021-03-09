# Fable.TsDeclaration

Fable plugin to emit [Typescript declarations](https://www.typescriptlang.org/docs/handbook/declaration-files/introduction.html) to interact with JS/TS from a Fable Elmish apps.

## Usage

Add a reference to the package:

```bash
dotnet add package Fable.TsDeclaration

# Or, if you use Paket:
dotnet paket add Fable.TsDeclaration
```

In your F# application, say, `App.fs`:

```fsharp
// Your Elmish types
type Model =
    { Names: string[]
      IsLoading: bool }

type Msg =
    | DoSomething of times: int
    | DoSomethingElse of instruction: string

// Binding to the JS component
// Learn more about importing JS code from Fable: https://fable.io/docs/communicate/js-from-fable.html
let initComponent(m: Model) (d: TsDispatcher<Msg>): unit = importMember "./component.js"

// In one point of your application
TsDeclaration.makeDispatcher model dispatch ||> initComponent
```

This will generate an `App.fs.d.ts` file as follows (the declaration will be automatically updated in watch compilations):

```typescript
type Model = {
    Names: string[],
    IsLoading: boolean,
}

type Dispatch = {
    doSomething(times: int): void,
    doSomethingElse(instruction: string): void,
}
```

Note the dispatcher sent to JS is automatically transformed into a JS object with methods to construct the `Msg` union cases and dispatch them.
