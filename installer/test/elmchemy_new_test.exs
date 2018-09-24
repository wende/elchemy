defmodule ElmchemyNewTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  @test_directory "test_directory"

  test "generates valid structure" do
    result = capture_io(fn ->
      Mix.Tasks.Elchemy.New.run(["testssss", "--location",@test_directory])
    end)

    assert File.exists?("#{@test_directory}/mix.exs") #It should be always present
    assert String.contains?(result, "Project generated successfully") #It is printed last

    #cleanup
    File.rm_rf!(@test_directory)
  end

end
