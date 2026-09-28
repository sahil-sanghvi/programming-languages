⍝ (echo "⎕io ← 0" && cat a5.apl && cat a5_test.apl ) > _x && dyalogscript _x

test ← {
  ⍳50 :: ⍵ 0 '∊' (0 0 0 0)
  a ← 20 12 17 21 ≡ (⍎⍵) 'AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC'
  b ← 0 0 0 0 ≡ (⍎⍵) ''
  c ← 0 0 0 0 ≡ (⍎⍵) 'HELLO'
  d ← 0 0 1 0 ≡ (⍎⍵) 'G'
  v ← a b c d
  ⍵ v
}
⎕ ← '[csc330_tester]' (test 'count')
⎕ ← '[csc330_tester]' (test 'count_t')

test ← {
  ⍳50 :: ⍵ 0 '∊' (0 0 0 0)
  a ← 100 ≡ (⍎⍵) 'GCGCGCGCCCGGGGCCG'
  b ← 0 ≡ (⍎⍵) 'HELLO'
  c ← (200÷3) ≡ (⍎⍵) 5 10 10 5 / 'ACGT'
  v ← a b c
  ⍵ v
}
⎕ ← '[csc330_tester]' (test 'gc')
⎕ ← '[csc330_tester]' (test 'gc_t')

test ← {
  ⍳50 :: ⍵ 0 '∊' (0 0 0)
  a ← 1 ≡ (⍎⍵) 1 1⍴42
  b ← 1 ≡ (⍎⍵) 3 3⍴4 9 2 3 5 7 8 1 6
  c ← 0 ≡ (⍎⍵) 2 2⍴1 2 3 4
  v ← a b c
  ⍵ v
}
⎕ ← '[csc330_tester]' (test 'magic')
⎕ ← '[csc330_tester]' (test 'magic_t')
