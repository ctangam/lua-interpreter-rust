::label1::
print("block: 1")
goto label2
::label3::
print("block: 3")
goto label4
::label2::
do
  print("block: 2")
  goto label3 -- goto outer block
end
::label4::
print("block: 4")