using Scratch, Test, Dates, Pkg
include("utils.jl")

@testset "Scrach Space Basics" begin
    # Run everything in a separate depot, so that we can test GC'ing and whatnot
    temp_pkg_dir() do project_dir
        # Create a global scratch space, ensure it exists and is writable
        dir = get_scratch!("test")
        @test isdir(dir)
        @test startswith(dir, scratch_dir())
        touch(joinpath(dir, "foo"))
        @test readdir(dir) == ["foo"]

        # Test that this created a `scratch_usage.toml` file, and that accessing it
        # again does not increase the size of the scratch_usage.toml file, since we
        # only mark usage once every so often per julia session.
        usage_path = joinpath(first(Base.DEPOT_PATH), "logs", "scratch_usage.toml")
        @test isfile(usage_path)
        size = filesize(usage_path)
        dir = get_scratch!("test")
        @test size == filesize(usage_path)

        # But accessing it from a new Julia instance WILL increase its size:
        code = "import Scratch; Scratch.get_scratch!(\"test\")"
        run(setenv(
            `$(Base.julia_cmd()) --project=$(dirname(@__DIR__)) -e $code`,
            "JULIA_DEPOT_PATH" => first(Base.DEPOT_PATH),
        ))
        @test size < filesize(usage_path)

        # Delete the scratch space, ensure it's gone.
        delete_scratch!("test")
        @test !isdir(dir)
    end
end

@testset "Scratch Space Namespacing" begin
    temp_pkg_dir() do project_dir
        # ScratchUsage UUID
        su_uuid = "93485645-17f1-6f3b-45bc-419db53815ea"
        # The UUID that gets used when no good UUIDs are available
        global_uuid = string(Base.UUID(UInt128(0)))

        # Touch the spaces of a ScratchUsage v1.0.0
        install_test_ScratchUsage(project_dir, v"1.0.0")

        # Ensure that the files were created for v1.0.0
        @test isfile(scratch_dir(su_uuid, "1.0.0", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "1.0.0"))) == 1
        @test isfile(scratch_dir(su_uuid, "1", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "1"))) == 1
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(global_uuid, "GlobalSpace"))) == 1

        # Next, do the same but for more versions
        install_test_ScratchUsage(project_dir, v"1.1.0")
        install_test_ScratchUsage(project_dir, v"2.0.0")

        # Check the spaces were shared when they should have been, and not when they shouldn't
        @test isfile(scratch_dir(su_uuid, "1.0.0", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "1.0.0"))) == 1
        @test isfile(scratch_dir(su_uuid, "1.1.0", "ScratchUsage-1.1.0"))
        @test length(readdir(scratch_dir(su_uuid, "1.1.0"))) == 1
        @test isfile(scratch_dir(su_uuid, "2.0.0", "ScratchUsage-2.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "2.0.0"))) == 1
        @test isfile(scratch_dir(su_uuid, "1", "ScratchUsage-1.0.0"))
        @test isfile(scratch_dir(su_uuid, "1", "ScratchUsage-1.1.0"))
        @test length(readdir(scratch_dir(su_uuid, "1"))) == 2
        @test isfile(scratch_dir(su_uuid, "2", "ScratchUsage-2.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "2"))) == 1
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.0.0"))
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.1.0"))
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-2.0.0"))
        @test length(readdir(scratch_dir(global_uuid, "GlobalSpace"))) == 3

        clear_scratchspaces!(Base.UUID(su_uuid))
        @test !isdir(scratch_dir(su_uuid))
    end
end


@testset "Scratch Space Lifecycling" begin
    temp_pkg_dir() do project_dir
        # First, install ScratchUsage
        su_uuid = "93485645-17f1-6f3b-45bc-419db53815ea"
        global_uuid = string(Base.UUID(UInt128(0)))
        install_test_ScratchUsage(project_dir, v"1.0.0")

        # Ensure that a few files were created
        @test isfile(scratch_dir(su_uuid, "1.0.0", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(su_uuid, "1.0.0"))) == 1
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.0.0"))
        @test length(readdir(scratch_dir(global_uuid, "GlobalSpace"))) == 1

        # Test that a gc() doesn't remove anything, and that there is no orphanage
        Pkg.gc()
        orphaned_path = joinpath(first(Base.DEPOT_PATH), "logs", "orphaned.toml")
        @test isfile(scratch_dir(su_uuid, "1.0.0", "ScratchUsage-1.0.0"))
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.0.0"))
        @test !isfile(orphaned_path)

        # Remove ScrachUsage, which causes the package (but not the scratch dirs)
        # to move to the orphanage
        Pkg.rm("ScratchUsage")
        rm(joinpath(project_dir, "ScratchUsage"); force=true, recursive=true)
        Pkg.gc()

        @test isfile(scratch_dir(su_uuid, "1.0.0", "ScratchUsage-1.0.0"))
        @test isfile(scratch_dir(global_uuid, "GlobalSpace", "ScratchUsage-1.0.0"))
        @test isfile(orphaned_path)
        orphanage = Pkg.TOML.parse(String(read(orphaned_path)))
        @test haskey(orphanage, scratch_dir(su_uuid, "1.0.0"))
        @test haskey(orphanage, scratch_dir(su_uuid, "1"))
        @test !haskey(orphanage, scratch_dir(global_uuid, "GlobalSpace"))

        # Run a GC, forcing collection to ensure that everything in the SpaceUsage
        # namespace gets removed (but still appears in the orphanage)
        sleep(0.2)
        Pkg.gc(;collect_delay=Millisecond(100))
        @test !isdir(scratch_dir(su_uuid))
        @test isdir(scratch_dir(global_uuid, "GlobalSpace"))
        orphanage = Pkg.TOML.parse(String(read(orphaned_path)))
        @test haskey(orphanage, scratch_dir(su_uuid, "1.0.0"))
        @test haskey(orphanage, scratch_dir(su_uuid, "1"))
        @test !haskey(orphanage, scratch_dir(global_uuid, "GlobalSpace"))
    end
end
