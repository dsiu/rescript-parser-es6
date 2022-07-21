open Main

Ava.test("parer", t => {
  let r = parse("110100101111111000101000")
  t->Ava.is(r->Belt.Result.isOk, true, ())
})
