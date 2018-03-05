module REPLTests

using Pkg3
using UUIDs
using Test

include("utils.jl")

const TEST_PKG = (name = "Example", uuid = UUID("7876af07-990d-54b4-ab0e-23690620f79a"))

temp_pkg_dir() do project_path
    cd(project_path) do
        @show LOAD_PATH
        pkg"init"
        pkg"add Example"
        @test isinstalled(TEST_PKG)
    end
end

end