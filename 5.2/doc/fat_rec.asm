
function main(0):
	CLOSURE  	$0, fat
	SETTABUP 	_ENV, fat, $0
	GETTABUP 	$0, _ENV, print
	GETTABUP 	$1, _ENV, fat
	LOADK    	$2, 5
	CALL     	$1, 2, 0
	CALL     	$0, 0, 1
	RETURN   	$0, 1

function fat(1):
	EQ       	0, $0, 0
	JMP      	0, label1 ; jump to label1
	LOADK    	$1, 1
	RETURN   	$1, 2
label1:	GETTABUP 	$1, _ENV, fat ; create label1
	SUB      	$2, $0, 1
	CALL     	$1, 2, 2
	MUL      	$1, $0, $1
	RETURN   	$1, 2
	RETURN   	$0, 1

