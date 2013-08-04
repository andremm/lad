
function main(0):
	1	[1]	GETTABUP 	$0, _ENV, "print"
	2	[1]	LOADK    	$1, "hello world!"
	3	[1]	CALL     	$0, 2, 1
	4	[1]	RETURN   	$0, 1
