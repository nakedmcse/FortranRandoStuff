program omp_test
    use OMP_LIB
    implicit none

    INTEGER :: partial_Sum, total_Sum, i

    !$OMP PARALLEL PRIVATE(partial_Sum) SHARED(total_Sum)
    partial_Sum = 0;
    total_Sum = 0;
    print *, OMP_GET_THREAD_NUM()

    !$OMP DO
    DO i=1,1000
        partial_Sum = partial_Sum + i
    END DO
    !$OMP END DO

    !$OMP CRITICAL
    total_Sum = total_Sum + partial_Sum
    !$OMP END CRITICAL

    !$OMP END PARALLEL
    PRINT *, "Total Sum: ", total_Sum
end program omp_test