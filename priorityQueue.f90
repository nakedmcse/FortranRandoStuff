program priority_queue_example
    implicit none

    integer, parameter :: range = 1000000
    integer, dimension(:, :), allocatable :: simple_prio, simple_prio_2
    integer :: i, priority, value, count, count_start, count_end, rate
    real :: start_time, end_time, elapsed
    integer, dimension(:), allocatable :: test_out

    ! Get clock rate
    call system_clock(count_rate=rate)

    ! Priority Queue Init
    allocate(simple_prio(2, 0))

    call enqueue(simple_prio, -1, 1)
    call enqueue(simple_prio, -1, 2)
    call enqueue(simple_prio, -1, 3)
    call enqueue(simple_prio, -5, 4)
    call enqueue(simple_prio, -9, 5)

    ! Dequeue and check order
    allocate(test_out(0))
    do while(size(simple_prio, 2) > 0)
        call dequeue(simple_prio, priority, value)
        call append(test_out, value)
    end do
    print *, "Simple Priority Queue Dequeued:", test_out

    ! Speed test for enqueue
    allocate(simple_prio_2(2, 0))
    call system_clock(count_start)
    do i = 1, range
        call enqueue(simple_prio_2, -int(rand() * 10), int(rand() * range))
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(rate)
    print *, "Simple priority queue enqueue time:", elapsed

    ! Speed test for dequeue
    count = 0
    count_start = 0
    count_end = 0
    call system_clock(count_start)
    do while(size(simple_prio_2, 2) > 0)
        call dequeue(simple_prio_2, priority, value)
        count = count + 1
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(rate)
    print *, "Simple priority queue dequeued items:", count
    print *, "Simple priority queue dequeue time:", elapsed

contains

    ! Heap enqueue
    subroutine enqueue(queue, prio, val)
        integer, dimension(:, :), intent(inout), allocatable :: queue
        integer, intent(in) :: prio, val
        integer, dimension(:, :), allocatable :: temp_queue
        integer :: n, i, parent, child

        ! Copy the existing queue to a temporary queue with one extra space
        n = size(queue, 2)
        allocate(temp_queue(2, n + 1))
        do i = 1, n
            temp_queue(:, i) = queue(:, i)
        end do
        if (allocated(queue)) then
            deallocate(queue)
        end if
        allocate(queue(2, n + 1))
        queue = temp_queue
        deallocate(temp_queue)

        ! Add new element to the end of the queue
        queue(:, n + 1) = [prio, val]

        ! Bubble up
        child = n + 1
        do while (child > 1)
            parent = child / 2
            if (queue(1, parent) > queue(1, child)) then
                call swap(queue(:, parent), queue(:, child))
                child = parent
            else
                exit
            end if
        end do
    end subroutine enqueue

    ! Heap dequeue
    subroutine dequeue(queue, prio, val)
        integer, dimension(:, :), intent(inout), allocatable :: queue
        integer, intent(out) :: prio, val
        integer, dimension(:, :), allocatable :: temp_queue
        integer :: n, parent, child, i

        ! Get the priority and value of the root
        prio = queue(1, 1)
        val = queue(2, 1)

        ! Replace root with the last element
        n = size(queue, 2)
        queue(:, 1) = queue(:, n)

        ! Create a new array with one less element and copy the elements
        if (n > 1) then
            allocate(temp_queue(2, n - 1))
            do i = 1, n - 1
                temp_queue(:, i) = queue(:, i)
            end do
            deallocate(queue)
            allocate(queue(2, n - 1))
            queue = temp_queue
            deallocate(temp_queue)
        else
            deallocate(queue)
            allocate(queue(2, 0))
        end if

        ! Bubble down
        parent = 1
        do while (2 * parent <= n - 1)
            child = 2 * parent
            if (child < n - 1 .and. queue(1, child + 1) < queue(1, child)) then
                child = child + 1
            end if
            if (queue(1, parent) > queue(1, child)) then
                call swap(queue(:, parent), queue(:, child))
                parent = child
            else
                exit
            end if
        end do
    end subroutine dequeue

    subroutine swap(a, b)
        integer, dimension(2), intent(inout) :: a, b
        integer, dimension(2) :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap

    ! Append to dynamic array
    subroutine append(arr, val)
        integer, dimension(:), intent(inout), allocatable :: arr
        integer, intent(in) :: val
        integer, dimension(:), allocatable :: temp_arr
        integer :: n, i

        ! Store the current size of the array
        n = size(arr)

        ! Allocate a temporary array with one additional element
        allocate(temp_arr(n + 1))

        ! Copy the contents
        do i = 1, n
            temp_arr(i) = arr(i)
        end do

        ! Add the new value to the end of the temp array
        temp_arr(n + 1) = val

        ! Deallocate the original array if it is allocated
        if (allocated(arr)) then
            deallocate(arr)
        end if

        ! Allocate the original array with the new size and copy back from temp
        allocate(arr(n + 1))
        arr = temp_arr

        ! Deallocate the temp
        deallocate(temp_arr)
    end subroutine append

end program priority_queue_example