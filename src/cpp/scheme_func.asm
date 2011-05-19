.686P
.XMM
include listing.inc
.model	flat
INCLUDELIB MSVCRTD
INCLUDELIB OLDNAMES
PUBLIC	_scheme_func
_TEXT	SEGMENT
_scheme_func	PROC
mov eax, 168
add eax, 4
ret 0
_scheme_func	ENDP
_TEXT	ENDS
END
