# Next Steps for Flips

> Published: 2020-11-21

Since Flips first released, I have been amazed at the support I have received from the F# community. I was incredibly intimidated to put out my work. I look up to so many people and I was not sure that my little contribution would amount to much. To my surprise, the work has been welcomed and I received significant help from people I have long admired. It is exciting and humbling to get to work with some of your heroes.

We have been using this library in production environments where billions of dollars are on the line and we wanted to offer it to the community. The great thing about exposing it to the wider community is that we get feedback from a far greater variety of use cases. As time has gone on, it has become clear that Flips needs to be refactored to enable it to grow and become a more powerful tool. I want to lay out the original vision of Flips, point out some of the design mistakes, and provide some insight into how we want to fix it. My hope is that by opening up about this process, future library authors can be saved from making these simple but painful mistakes.

## The Origin of Flips

I want to tell you a little of the history of Flips to provide the historical context behind some of the design decisions. If you want to skip this story, please feel free to jump to the next [section](#Where-We-Are-Now). Flips is the third generation of this library. Three years ago, I, Matthew Crews, was attending a training put on by [Gurobi](https://www.gurobi.com/). For those of you who do not know, Gurobi is the company which produces the best mathematical solver in the world. Many of the PhDs who wrote the papers on Mathematical Optimization Solvers work there. While the academic credentials are prestigious, the people are incredibly warm and inviting. 

During this training, I had the pleasure of sitting down with Dr. Glockner who was one of the authors of Gurobi's Python library. I was expressing how much I loved F# as a language and he posed me a little challenge. There was an aspect of the Python library that had been incredibly difficult to implement. He wondered if it could be done in F#. That night I went back to my room and stayed up till 1:00 AM trying. My solution fell short, but the challenge stuck with me. I ended up creating an F# wrapper around the Gurobi .NET library as a thought experiment. I point to that tiny library as the grandfather of Flips.

A year later I am hired by Quicken Loans. They were looking for an F# developer and someone with a background in Mathematical Planning was a bonus. When I started, they were writing services in F# but the Mathematical Planning Model was written in C#. The library they were using was heavily Object-Oriented. I did not like that almost the entire stack was F# except this one tiny piece. I made a second attempt at writing a F# library for Mathematical Planning. We called it Mipy and we began using it in production. To be frank, the library was terrible, and I am glad the community will never see it. I think it is important that people know that Flips was born out of several failures.

Fast forward another year and a half and I attend an NDC Conference in Minnesota where I take a workshop by [Mark Seemann](https://blog.ploeh.dk/). During this workshop I learn new concepts that illuminate mistakes that I made with Mipy. I go back and look at the problem with new eyes and start over, again. I focus heavily on the ergonomics of the library. This fresh re-write I call Flips (F# Linear Programming System). I am armed with new concepts which help me avoid some of the pitfalls I fell into before. I make some new mistakes (we will get to that below) but the overall experience of composing models is much better. I even finally solve the challenge posed to me by Dr. Glockner. The answer was Computation Expressions, and you can look at the `ConstraintBuilder` for how it was solved. Fresh off the high of making something so much cleaner, I decide to take the plunge and open the library up to the world.

## Where We Need to Go

Despite Flips being a vast improvement from previous versions of this idea, some critical errors were made. We need to fix them so that Flips can evolve more smoothly. The first mistake was releasing a `1.0.0` version too early. As a first-time library author, I was too excited to get my work out there. If I were still on a `0.X.X` version, I would feel less reticence about evolving the API. I own this mistake and I want to fix the API now rather than let it fester.

### Mistake 1: Exposing the `SolverSettings` type

This one surprised me but has become one of the biggest sticking points. When Flips was written, all the emphasis was on making the modeling of problems simple and easy. Less focus was put on the solve and solution extractions steps. Flips is meant to be a simple tool to use so the thought was that we would only expose few configuration settings for the underlying solvers. These settings are configured by creating a `SolverSettings` type and passing that to the `Solver.solve` function. Creating this type uses record construction:

```fsharp
type SolverSettings = {
    SolverType : SolverType
    MaxDuration : int64
    WriteLPFile : Option<string>
    WriteMPSFile : Option<string>
}

let settings : SolverSettings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}
```

This may not seem bad but what happens the moment you want to add a field to the `SolverSettings` type? You immediately break client's code because now wherever they are creating that type, they need to add the new field. We need to be able to add settings without breaking client code. Note to future library authors, be cognizant of how users of your library will be able to create instances of types. This choice was shortsighted and needs to be fixed in the next major release.

## Mistake 2: Using a Discriminated Union Instead of an Interface for Solver

Right now, when you want to use different underlying solvers, you select a case of the `SolverType` discriminated union. When the `Solver.solve` function is called, it sees which case you have selected and uses the appropriate mapping from the Flips model to the underlying solver's model. This causes two problems. The first is that Flips must carry all the code to support all of the possible solver backends. This adds bloat to the library. If a user only intends on using a single solver backend, they should not carry the weight of every other.

The second problem is that adding solver specific settings becomes cumbersome. The Gurobi, CPLEX, CBC, and GLOPS solvers all have different features. Do we only expose the ones that are shared across all? Do we perform a weird hack which ensure you are only using settings that correspond to the `SolverType` case you used? It gets messy and unintuitive. Up till now, only features that are shared across all backends were even considered. This prevents expert users from harnessing the full power of the solvers though.

Thankfully, [Guathier Segay](https://github.com/smoothdeveloper), has made some excellent suggestions that I think should enable Flips to evolve smoothly. Instead of a discriminated union, define an interface `ISolver` in base Flips and provide instances in solver specific libraries. An `ISolver` provides the base set of operations for submitting a model and receiving a solution back. A solver specific library provides an instance of `ISolver` while also exposing the configuration options for the specific underlying solver. This will allow us to slim down the Flips library while also enabling finer control for expert users. The only downside is that users need to install two Nuget packages instead of just one. One for base Flips and one for the solver backend they want to use. Ultimately this should lead to a cleaner solution.

## Mistake 3: SliceMaps in Flips

The SliceMap types have been another dream of mine since working with the Gurobi Python library. They provide an incredibly elegant way to compose optimization problems. When I first conceived of writing Flips, I did not consider that people outside of the Mathematical Planning domain would have any interest in these collection types. Because of that, I never thought about spinning them off as their own library. After Flips was published though, it was clear the SliceMaps could stand on their own. I have since created a [new repo](https://github.com/matthewcrews/SliceMap) for them where I will continue to flesh them out and publish them as their own library. I am putting this in as a mistake because SliceMaps will need to remain under the `Flips` namespace in order to not break client code.

## Conclusion

I am sure there are more mistakes that were made but these are the big ones we are wanting to address in the next major release. I believe by addressing these shortcomings now, it will pay dividends in the future. There are many exciting use cases we want to support that are just not feasible currently. The design ethos will remain the same though. We want to keep simple things simple while enabling complex things. Flips is meant to be welcoming to newcomers. My desire is that more people become aware of the wonderful tools that make up Mathematical Planning and find amazing ways to help others.
