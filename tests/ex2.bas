Dim A As Integer = 5
If A > 5 Then
	Print A
ElseIf A <= 5 Then
	If A < 8 Then
		Print "blabla"
	End If
End If
A = 3
Dim B As Integer = 12
Do
	Do
		Print "cc"
		A = A+1
	Loop While A < 5
	Do
		Print A
		A = A+1
	Loop While A < 8
	B = B-1
Loop While B > 10
