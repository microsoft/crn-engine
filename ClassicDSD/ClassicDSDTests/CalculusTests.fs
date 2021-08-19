module Microsoft.Research.DNA.CalculusTests

open FsUnit
open FsUnit.Xunit
open Xunit

open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

module Value = Microsoft.Research.CRNEngine.Expression

[<Trait("Category", "DSD")>]
let ``test`` () = 
  let text = """def input1()   = <a0^ s>
  def input2()   = <b0^ s>
  def fuel()     = <y^*>[s*]{x^>
  def origami()  = [[ {tether(a,b) a0^*}[s]{y^>
                    | {tether(c,d) b0^*}[s]{y^>
                    | {tether(a,c) x^*}[s]{blank^>
                    | {tether(b,d) x^*}[s]{y^> ]]

  ( input1()
  (*| input2()*)
  | 3 * fuel()
  | origami()
  )
"""
  let t'= SLConversion.convertSL text
  let t = Dsd.compile t'
//  Assert.NotSame("", t')
  Assert.NotEmpty( t.initials )

[<Trait("Category", "DSD")>]
[<Fact>]
let ``Rule: RB,N+``() =
    let text = "directive unproductive ({M N^* S} | <L N^ R>)"
    let t = Dsd.compile text
    
    Assert.NotEmpty(t.reactions)
    let isReversible = 
      match t.reactions.[0].reverse with
        | None   -> false
        | Some _ -> true
    Assert.True(isReversible)
    Assert.Equal(3, t.initials.Length) // 2 species above plus one resulting from their merging

    let crn = Dsd.convert_expand (Dsd.parse text)
    Assert.Equal(1, crn.reactions.Length)
    let isReversible2 = 
      match crn.reactions.[0].reverse with
        | None   -> false
        | Some _ -> true
    Assert.True(isReversible2)
    Assert.Equal(3, crn.initials.Length)

    let opts = Options.setUnproductive true Options.default_options
    let bind_rate   = Rate.MassAction (Value.Float (Options.get_toehold_bind_rate opts))
    let unbind_rate = Rate.MassAction (Value.Float (Options.get_toehold_unbind_rate opts))
    
    let seq (s1:Species) (s2:Species) = s1.name = s2.name 
    let reversibles = Crn.compute_reversibles seq t

    Assert.Equal(1, reversibles.Length)
    let rv = reversibles.[0]
    Assert.Equal(2, rv.reactants.Length)
    Assert.Equal(1, rv.products.Length)
    Assert.Empty(rv.catalysts)
    Assert.Equal(bind_rate, rv.rate)
    Assert.Equal(Some unbind_rate, rv.reverse)