      program outputdata
      implicit none

      !Variable Iniziation
      integer :: ndata, i, j, col, test
      real :: r
      real, dimension(135000,4) :: input_data
      integer, dimension(135000) :: neighbor_index
      real, dimension(1,4) :: point
      real, dimension(3,4) :: test_input

      !File Open
      open(2, file="kriging_parameter.txt") !File Parameter
      read (2,*) ndata
      close(2)
      
      write (*,*) ndata

      !Load File
      !allocate(input_data(ndata*3)) !Matrix xyzv data
      col = 4
      
      open(1, file="GeoSystemTopo01 - Copy.txt")

      do i = 1,135000
         read(1,*) input_data(i,:)
      enddo
      
      close(1)
      
      !Data Selection
      point(1,:) = input_data(1,:)
      call edistance(input_data(1200,:), point(1,:))

      write(*,*) r
      
      !Penahan Terminal
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
      
      !End Program
      end program outputdata
