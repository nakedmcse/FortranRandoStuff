program datagen_mt
    use OMP_LIB
    implicit none
    character(len=100) :: line
    character(len=20), dimension(50000) :: weather_stations
    integer, dimension(100000,2) :: chunk
    integer :: i, j, c, ios, num_stations
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
    print *, 'Generating Data'
    open(unit=20, file='output_data.csv', status='unknown', position='append', iostat=ios)
    do c = 1, output_lines/chunk_size
        !$OMP PARALLEL SHARED(chunk) PRIVATE(j)
        !$OMP DO
        do j = 1, chunk_size
            ! Add random weather station and temp to chunk
            call random_number(rnd)
            chunk(j,1) = int(rnd * num_stations) + 1
            chunk(j,2) = int(rnd * 200) - 100
        end do
        !$OMP END DO
        !$OMP END PARALLEL
        do i = 1, chunk_size
            ! Write chunk
            write(20,'(A, A, I0)') trim(weather_stations(chunk(i,1))), ';', chunk(i,2)
        end do
        ! Update progress
        progress = (real(c)/real(output_lines/chunk_size)) * 100
        write(*, '(A, A, I3, A)', advance='no') char(13), ' Progress: ', int(progress), '%'
    end do
    close(20)
    print *
    print *, 'Data Generated'

    contains

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