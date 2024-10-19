program datagen_mt
    use OMP_LIB
    implicit none
    character(len=100) :: line
    character(len=20), dimension(50000) :: weather_stations
    character(len=30), dimension(100000) :: chunk
    integer :: i, j, c, ios, num_stations, rnd_choice, rnd_temp, chunk_start, chunk_end, thread_id
    integer :: output_lines = 100000000
    integer :: chunk_size = 100000
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
    call OMP_SET_NUM_THREADS(4)
    print *, 'Generating Data'
    open(unit=20, file='output_data.csv', status='unknown', position='append', iostat=ios)
    do c = 1, output_lines/chunk_size
        !$OMP PARALLEL SHARED(chunk, weather_stations) PRIVATE(rnd_choice, rnd_temp, rnd, chunk_start, chunk_end, thread_id, j)
        thread_id = OMP_GET_THREAD_NUM()
        select case (thread_id)
            case (0)
                chunk_start = 1
                chunk_end = 25000
            case (1)
                chunk_start = 25001
                chunk_end = 50000
            case (2)
                chunk_start = 50001
                chunk_end = 75000
            case (3)
                chunk_start = 75001
                chunk_end = 100000
        end select

        !$OMP DO
        do j = chunk_start, chunk_end
            ! Add random weather station and temp to chunk
            call random_number(rnd)
            rnd_choice = int(rnd * num_stations) + 1
            rnd_temp = int(rnd * 200) - 100
            chunk(j) = trim(weather_stations(rnd_choice)) // ';' // adjustl(itoa(rnd_temp))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
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

end program datagen_mt