function UserInput(T=String,msg="")
  print("$msg ")
  if T == String
      return readline()
  else
    try
      return parse(T,readline())
    catch
     println("Sorry, I could not interpret your answer. Please try again")
     UserInput(T,msg)
    end
  end
end
