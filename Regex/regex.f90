! Regex integration
module regex
    use ISO_C_Binding
    use ISO_Fortran_Env

    type regex_t
        type(C_ptr) :: preg
    end type regex_t

    integer, parameter :: NOMATCH = -1

    interface
        subroutine api_alloc(preg_return) bind(C,name="api_alloc")
            import
            type(C_ptr) :: preg_return
        end subroutine api_alloc

        subroutine api_compile(preg,pattern,flags,status) bind(C,name="api_regcomp")
            import
            type(C_ptr), value :: preg
            character(len=1, kind=C_char) :: pattern(*)
            character(len=1, kind=C_char) :: flags(*)
            integer(C_int) :: status
        end subroutine api_compile

        subroutine api_exec(preg,string,nmatch,matches,flags,status) bind(C,name="api_regexec")
            import
            type(C_ptr), value :: preg
            character(len=1,kind=C_char) :: string(*)
            integer(C_int), value :: nmatch
            integer(C_int) :: matches(2,nmatch)
            character(len=1,kind=C_char) :: flags(*)
            integer(C_int) :: status
        end subroutine api_exec

        function api_error(errcode, preg, errbuf, errbuf_size) result(regerror) bind(C,name="regerror")
            import
            integer(C_size_t) :: regerror
            integer(C_int), value :: errcode
            type(C_ptr), value :: preg
            character(len=1,kind=C_char) :: errbuf
            integer(C_size_t), value :: errbuf_size
        end function api_error

        subroutine api_free(preg) bind(C,name="regfree")
            import
            type(C_ptr), value :: preg
        end subroutine api_free
    end interface
    contains
    subroutine regex_compile(preg, pattern, flags, status)
        type(regex_t) :: preg
        character(len=*) :: pattern
        character(len=*), optional :: flags
        integer, optional :: status
        integer(C_int) :: status_local
        character(len=10,kind=C_char) :: flags_local

        flags_local = merge(flags, ' ', present(flags))
        preg%preg = C_NULL_ptr
        call api_alloc(preg%preg)
        call api_compile(preg%preg, trim(pattern)//C_NULL_char, trim(flags_local)//C_NULL_char, status_local)
        if (present(status)) then
            status = status_local
        else if (status_local /= 0) then
            stop 'Regex Compile Failed'
        end if
    end subroutine regex_compile

    function regex_exec(preg, s, matches, flags, status) result(match)
        logical :: match
        type(regex_t) :: preg
        character(len=*) :: s
        character(len=*), optional :: flags
        integer, optional :: status, matches(:,:)
        integer(C_int) :: status_local, matches_local(2,1)
        character(len=10,kind=C_char) :: flags_local

        matches = NOMATCH
        flags_local = merge(flags, ' ', present(flags))
        if (present(matches)) then
            call api_exec(preg%preg, trim(s)//C_NULL_char, size(matches,2), matches, trim(flags_local)//C_NULL_char, status_local)
        else
            call api_exec(preg%preg, trim(s)//C_NULL_char, int(0,C_int), matches_local, trim(flags_local)//C_NULL_char, status_local)
        end if
        match = status_local==0
        if (present(status)) then
            status=status_local
        else if (status_local/=0 .and. status_local/=1) then
            stop 'Regex Exec Failed'
        end if
    end function regex_exec

    function regex_match(match,s,matches) result (result)
        integer :: match, matches(2,*)
        character(len=*) :: s
        character(len=matches(2,match)-matches(1,match)) :: result
        result = s(matches(1,match)+1:matches(2,match))
    end function regex_match

    subroutine regex_error(preg,code,msg,msg_len)
        type(regex_t) :: preg
        integer :: code, msg_len
        character :: msg
        msg_len = api_error(int(code,C_int), preg%preg, msg, int(len(msg),C_size_t))
    end subroutine regex_error

    subroutine regex_free(preg)
        type(regex_t) :: preg
        call api_free(preg%preg)
        preg%preg = C_NULL_ptr
    end subroutine regex_free
end module regex