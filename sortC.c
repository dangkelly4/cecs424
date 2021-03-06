#include <stdio.h>
#include <stdlib.h>

//basic swap method
void swap(int *a, int *b)
{
	int temp = *a; //set value of temp to first position being swapped
	*a = *b; //set value of first position to the number it is being swapped with
	*b = temp; // set value of second position to first position
}

//quick sort function
void qsort2(int *a, int n){
    if(n<2) return; //return if only one element in array
    int pivot = a[n/2]; //setting pivot; middle element of the array
    int *left = a; //setting index of left subarray
    int *right = a + n - 1; //setting index of right subarray
    
    while(left <= right){
        //if a number is less than the pivot of the array, move on to the next position until a number greater than the pivot is found
        if (*left < pivot)
            left++;
        //if a number is greater than the pivot of the array, decrement to check the next element 
        else if (*right > pivot)
            right--;
        //swap elements
        else {
            swap(left, right);
            left++;
            right--;
      
        }
    }

    //recursive call
    qsort2(a, right - a + 1);
    qsort2(left, a + n - left);
}


//merge sort function
void mergeHelper(int *a,int start, int mid, int end){
  int temp[end-start + 1];//set temporary variable

  //initalize variables
  int i = start;
  int j = mid + 1;
  int k = 0;

//while i is less than the middle element and j is less than the last element, merge elements into the temporary array
  while(i<=mid && j <= end){
    //if the first element of the list is less than the last element of the list, assign the middle element into the first place and then increment to go through the list
    if(a[i] < a[j]){
      temp[k] = a[i];
      i++;
  //if the first element is greater than the last element, assign the middle element into the last place and then increment to go through the list
    } else {
      temp[k] = a[j];
      j++;
    }
  }
  
  //if i is less than the mid.. we would assign the middle element into the last element
  //increment dump rest of list into the remainder of temp
  if(i<mid){
    while(j<=start){
      temp[k]=a[j];
      j++;
      k++;
    } 
  } else {
    //while i is less than or greater to the middle element, assign the middle element into the first element of the list
    //increment and dump the rest of the list into the remainder of temp 
      while(i<=mid){
        temp[k] = a[i];
        i++;
        k++;
  }
  //loop through the temp array
  for(i = start; i<=end; i++)
      a[i] = temp[i-start]; // replace all the elements in "a" with elements in "temp"
  }
}

void mergesort (int *a, int start, int end){
  //if the end element is less than the starting element, we would merge!
  if(end<start){
    int mid = (start+end)/2; //inizialize middle element

    mergesort(a, start,mid);//recursive; offers an array, the starting element and the middle element
    mergesort(a, mid + 1, end); //recursive; offers an array, the middle element, and the last element
    mergeHelper(a, start, mid, end); //merges the list by combining the first, middle, and last elements
 }
}

void msort(int*a, int n){
  mergesort(a, 0, n-1);//calls mergesort offering the array, the initial position and the end position
}

void printArr(int *a, int n){
  printf("[ ");
  for (int i=0; i<n; i++){ //for loop to go through all the arrays positions
	printf("%d ",a[i]);	//printing each element of the array
	}
	printf("]\n");//skipping
}


int main(){
  //given array
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782,1};
  
  //print array sorted by quick sort
    printf("Quick Sort: ");
    qsort2(a,10);
    printArr(a,10);
    
  //print array sorted by merge sort
    printf("Merge Sort: ");
    msort(a,10);
    printArr(a,10);
}
    
