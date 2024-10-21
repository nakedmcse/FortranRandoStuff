program hashmap_play
    use stdlib_hashmaps, only: chaining_hashmap_type, open_hashmap_type
    use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set, get
    implicit none
    type(chaining_hashmap_type), allocatable :: map
    type(key_type) :: key
    type(other_type) :: data
    class(*), allocatable :: retrieved_data
    logical :: has_key, conflict
    type stats
        integer :: value(3)
    end type stats
    character(len=10) :: key_val
    type(stats) :: value
    type(stats) :: retrieved_value
    integer :: status

    allocate(map)
    call map%init(fnv_1_hasher)

    value%value = [-10, 5, 15]
    key_val = 'London'
    call set(data, value)
    call set(key, key_val)
    call map%map_entry(key, data, conflict)

    value%value = [15, 30, 60]
    key_val = 'Houston'
    call set(data, value)
    call set(key, key_val)
    call map%map_entry(key, data, conflict)

    value%value = [5, 10, 20]
    key_val = 'New York'
    call set(data, value)
    call set(key, key_val)
    call map%map_entry(key, data, conflict)

    key_val = 'London'
    call map%get_other_data(key_val, data)
    call get(data, retrieved_data)
    select type(retrieved_data)
        type is (stats)
            print *, 'London: ', retrieved_data%value
    end select

    key_val = 'Houston'
    call map%get_other_data(key_val, data)
    call get(data, retrieved_data)
    select type(retrieved_data)
    type is (stats)
        print *, 'Houston: ', retrieved_data%value
    end select

    key_val = 'New York'
    call map%get_other_data(key_val, data)
    call get(data, retrieved_data)
    select type(retrieved_data)
    type is (stats)
        print *, 'New York: ', retrieved_data%value
    end select

    deallocate(map)
end program hashmap_play