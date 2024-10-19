program datagen
    implicit none
    character(len=100) :: line
    character(len=20), dimension(50000) :: weather_stations
    character(len=30), dimension(1000000) :: chunk
    integer :: i, c, ios, num_stations, rnd_choice, rnd_temp
    integer :: output_lines = 100000000
    integer :: chunk_size = 1000000
    real :: rnd, progress

    ! Read weather stations
    print *, 'Reading Weather Station Data'
    num_stations = 0;
    open(unit=10, file='weather_stations.csv', status='old')
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (line(1:1) /= '#') then
            num_stations = num_stations + 1
            call get_station(line, weather_stations(num_stations))
        end if
    end do
    close(10)

    ! Loop chunks
    print *, 'Generating Data'
    open(unit=20, file='output_data.csv', status='unknown', position='append', iostat=ios)
    do c = 1, output_lines/chunk_size
        do i = 1, chunk_size
            ! Add random weather station and temp to chunk
            call random_number(rnd)
            rnd_choice = int(rnd * num_stations) + 1
            rnd_temp = int(rnd * 200) - 100
            chunk(i) = trim(weather_stations(rnd_choice)) // ';' // adjustl(itoa(rnd_temp))
        end do
        do i = 1, chunk_size, 4
            ! Write chunk
            write(20,'(A)') trim(chunk(i)) // new_line('A') // trim(chunk(i+1)) // new_line('A') &
                // trim(chunk(i+2)) // new_line('A') // trim(chunk(i+3))
        end do
        ! Update progress
        progress = (real(c)/real(output_lines/chunk_size)) * 100
        write(*, '(A, A, I3, A)', advance='no') char(13), ' Progress: ', int(progress), '%'
    end do
    close(20)
    print *
    print *, 'Data Generated'

    contains

        function itoa(i) result(res)
            integer :: i
            character(len=32) :: res
            write(res, '(I0)') i
        end function itoa

        subroutine get_station(line, name)
            character(len=*), intent(in) :: line
            character(len=20), intent(out) :: name
            integer :: p

            p = index(line, ';')
            if (p > 0) then
                name = line(1:p-1)
            else
                name = line
            end if
        end subroutine get_station

end program datagen