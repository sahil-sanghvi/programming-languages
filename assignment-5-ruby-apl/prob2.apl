⍝ V01039545 - Sahil Sanghvi
⍝ CSC 330 - Assignment 5, Problem 2: Deoxyribonucleic Arrays
⎕io ← 0
count ← { +⌿(,⍵)∘.='ACGT' }
count_t ← { +⌿(,⍵)∘.='ACGT' }
gc ← { 100×(+/⍵∊'CG')÷1⌈≢⍵ }
gc_t ← ( 100×(+/∊∘'CG')÷1⌈≢ )
magic ← { ∧/(⊃+/⍵)=(+/⍵),(+⌿⍵),(+/,⍵×∘.=⍨⍳≢⍵),(+/,⍵×⌽∘.=⍨⍳≢⍵) }
magic_t ← { ∧/(⊃+/⍵)=(+/⍵),(+⌿⍵),(+/,⍵×∘.=⍨⍳≢⍵),(+/,⍵×⌽∘.=⍨⍳≢⍵) }
