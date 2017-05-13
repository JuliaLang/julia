root = Trees.AryNode()
@test Trees.isroot(root)
@test eltype(root) == Nothing
c1 = Trees.AryNode(root, 7.3)
@test c1.parent == root
@test contains(root.children, c1)
c2 = Trees.AryNode(root, "cat")
c3 = Trees.AryNode(root, 4//3)
c4 = Trees.AryNode(root, [1 2;3 4])
c21 = Trees.AryNode(c2, -3)
@test contains(root.children, c2)
@test !contains(root.children, c21)
@test Trees.destroy!(c2) == "cat"
@test Trees.isroot(c2)
@test Trees.isroot(c21)
@test !contains(root.children, c2)
