program hashmap
    implicit none
    integer :: capacity = 1024

    type ht_entry
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        logical :: is_occupied = .false.
    end type ht_entry

    type ht
        type(ht_entry), dimension(:), allocatable :: entries
        integer :: length
    end type ht

    type(ht) :: hashtable

    ! Initialize the hash table
    call ht_create(hashtable, capacity)

    ! Add some key-value pairs
    call ht_set(hashtable, "one", "one's value")
    call ht_set(hashtable, "two", "two's value")
    call ht_set(hashtable, "three", "three's value")

    ! Retrieve and print the values
    print *, "Key 'one': ", trim(ht_get(hashtable, "one"))
    print *, "Key 'two': ", trim(ht_get(hashtable, "two"))
    print *, "Key 'three': ", trim(ht_get(hashtable, "three"))

    ! Destroy the hash table
    call ht_destroy(hashtable)

contains

    function hash_key(key) result(res)
        integer(kind=8) :: fnv_offset = 2166136261_8
        integer(kind=8) :: fnv_prime = 16777619_8
        character(len=*), intent(in) :: key
        integer(kind=8) :: res
        integer :: i
        res = fnv_offset
        do i = 1, len_trim(key)
            res = ieor(res, int(ichar(key(i:i)), kind=8))
            res = res * fnv_prime
        end do
    end function hash_key

    subroutine ht_create(table, cap)
        type(ht), intent(inout) :: table
        integer, intent(in) :: cap
        allocate(table%entries(cap))
        table%length = cap
    end subroutine ht_create

    subroutine ht_destroy(table)
        type(ht), intent(inout) :: table
        deallocate(table%entries)
        table%length = 0
    end subroutine ht_destroy

    subroutine ht_set(table, key, value)
        type(ht), intent(inout) :: table
        character(len=*), intent(in) :: key, value
        integer(kind=8) :: hash
        integer :: index, i

        hash = abs(hash_key(key))
        index = mod(hash, table%length) + 1 ! Ensure 1-based indexing
        print *, index, hash

        do i = 1, table%length
            if (.not. table%entries(index)%is_occupied .or. table%entries(index)%key == key) then
                ! Insert or update the entry
                if (allocated(table%entries(index)%key)) deallocate(table%entries(index)%key)
                if (allocated(table%entries(index)%value)) deallocate(table%entries(index)%value)

                allocate(character(len=len_trim(key)) :: table%entries(index)%key)
                allocate(character(len=len_trim(value)) :: table%entries(index)%value)

                table%entries(index)%key = trim(key)
                table%entries(index)%value = trim(value)
                table%entries(index)%is_occupied = .true.
                return
            end if
            index = mod(index, table%length) + 1 ! Linear probing
        end do

        print *, "Hash table is full!"
    end subroutine ht_set

    function ht_get(table, key) result(res)
        type(ht), intent(in) :: table
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: res
        integer(kind=8) :: hash
        integer :: index, i

        hash = abs(hash_key(key))
        index = mod(hash, table%length) + 1 ! Ensure 1-based indexing

        do i = 0, table%length - 1
            if (.not. table%entries(index)%is_occupied) then
                res = "" ! Key not found
                return
            end if
            if (table%entries(index)%key == key) then
                res = table%entries(index)%value
                return
            end if
            index = mod(index, table%length) + 1 ! Linear probing
        end do

        res = "" ! Key not found
    end function ht_get

end program hashmap
