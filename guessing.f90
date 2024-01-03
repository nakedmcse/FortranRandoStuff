program guessing_game
    implicit none
    integer :: guess, n, i, value
    logical :: play_again, done

    play_again = .true.
    done = .false.
    n = 0

    print *,'Guess the number between 1 and 100'
    do while(done .eqv. .false.)
        if(play_again .eqv. .true.) then
            value = int(rand() * 100)+1
            play_again = .false.
        end if
        print *,'Enter your guess:'
        read (*,*) guess
        if(guess > value) then
            print *,'Too large, try again'
            n = n + 1
        elseif(guess < value) then
            print *,'Too small, try again'
            n = n + 1
        else
            print *,'You guessed it in ', n
            print *,'Play again (1, yes, 0, no) ?'
            read (*,*) i
            if(i == 0) then
                done = .true.
            else
                play_again = .true.
            end if
        end if
    end do
end program