#include <math.h>
Dim A As Double = 2.2
Function TestFunc (A As Double) As String
	Print A
	Return "salut"
End Function
Print A
Sub Test (v As Integer, b As String)
	If v > 10 Then
		Print "Vais-je par là ? "
	End If
	/' quelle belle fonction ! 
		test
	'/
	Print b
End Sub
Dim v As Integer, b As String
v = 5: b="toto"
Test (v, b)
