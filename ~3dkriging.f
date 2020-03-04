      program outputdata
      implicit none

      !Variable Iniziation
      integer :: h, i, j, col, s, test, nx, ny, nz
      real :: r, block_size, block_x, block_y, block_z
      real, dimension(8) :: param
      real, dimension(135000,4) :: input_data, temp_selected_data
      real, dimension(:,:), allocatable :: selected_data, grid
      integer, dimension(135000) :: index

      !Parameter File
      !ndata = param(1), xmin = param(2), xmax = param(3)
      !ymin = param(4), ymax = param(5), zmin = param(6), zmax = param(7)
      !grid size = param(8)
      
      open(2, file="kriging_parameter.txt") !File Parameter

      do i = 1,8
         read (2,*) param(i)
      enddo
      
      close(2)

      !XYZV File
      !allocate(input_data(ndata*3)) !Matrix xyzv data
      open(1, file="GeoSystemTopo01 - Copy.txt")
      
      do i = 1,135000
         read(1,*) input_data(i,:)
      enddo
      
      close(1)
      
      !Data Selection (x, y, min max of desired grid)
      col = 0
      s = 1
      
      do j = 1,135000
            do i = s,135000
               if (input_data(i,1) > param(2)) then
               if (input_data(i,1) < param(3)) then
               if (input_data(i,2) > param(4)) then
               if (input_data(i,2) < param(5)) then
               if (input_data(i,3) < param(6)) then
               if (input_data(i,3) > param(7)) then
                  col = col + 1
                  s = i + 1
                  index(j) = i
                  exit
               endif
               endif
               endif
               endif
               endif
               endif
             enddo

             if (i == 135001) then
                exit
             endif
      enddo
      
      write(*,*) i
      write(*,*) s
      write(*,*) col
      write(*,*) index(col)
      
      !Insert Selected Data
      allocate(selected_data(col,4))
      
      do i = 1,col
         selected_data(i,:) = input_data(index(i),:)
      enddo

      !Mesh Grid
      
      !Block Calculation
      nx = int((param(3) - param(2))/param(8))
      block_x = (param(3) - param(2))/nx

      ny = int((param(5) - param(4))/param(8))
      block_y = (param(5) - param(4))/ny
      
      nz = int((param(6) - param(7))/param(8))
      block_z = (param(6) - param(7))/nz
      
      !Grid Creation
      allocate(grid(nx*ny*nz,4))
      
      !Grid Coordinate
      do h = 1, nz
         do i = 1, ny
            do j = 1, nx
        grid(nx*ny*(h-1) + nx*(i-1) + j, 1) = param(2) + block_x*(j-1)
        grid(nx*ny*(h-1) + nx*(i-1) + j, 2) = param(4) + block_y*(i-1)
        grid(nx*ny*(h-1) + nx*(i-1) + j, 3) = param(7) + block_z*(h-1)
            enddo
         enddo
      enddo
      
      !Cek Speed
      do i = 1, nx*ny*nz
         do j = 1, col
            call edistance(grid(i,:), selected_data(j,:))
         enddo
      enddo

      !Test Write File

      !End Program
      write(*,*) "End of Program"
      read *, test
      
      contains
      !Subroutine For Calculating distance of 2 points
      subroutine edistance(point1, point2)
         real, dimension(1,4) :: point1, point2
         
         r = 0
         r = r + (point1(1,1) - point2(1,1))**2
         r = r + (point1(1,2) - point2(1,2))**2
         r = r + (point1(1,3) - point2(1,3))**2
         r = r**0.5
         
      end subroutine edistance
      
      end program outputdata
