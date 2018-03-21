/* dskconv - converts Semper (tm) disks between PC and SG formats
 * (c) G. Chand, W.O. Saxton 1994,2001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BLKSIZE 64
#define IM_BYTE 0
#define IM_INTEGER 1
#define IM_FP   2
#define IM_COMPLEX 3

typedef unsigned char BYTE;
typedef short INT2;
typedef int INT4;
typedef int BOOL;

#define FALSE 0
#define TRUE 1

/* disc header - first 64-byte block */
typedef struct {
  BYTE id[11];
  BYTE major_version;
  BYTE minor_version;
  BYTE file_size[3];
  BYTE dir_size[2];
  BYTE pad[46];
} header;

/* directory slot - segment descriptor (SD) */
typedef struct {
  INT4 number;
  INT4 location;
} pic_info;

/* picture label */
typedef struct {
  BYTE id[6];
  BYTE lbnc1;
  BYTE lbnc2;
  BYTE lbnr1;
  BYTE lbnr2;
  BYTE lbnl1;
  BYTE lbnl2;
  INT2 lbcc;
  INT2 lbcr;
  INT2 lbcl;
  BYTE lbclas;
  BYTE lbform;
  BYTE lbwp;
  BYTE lbyear;
  BYTE lbmon;
  BYTE lbday;
  BYTE lbhour;
  BYTE lbmin;
  BYTE lbsec;
  BYTE lbncrr;
  INT2 lbrr;
  BYTE lbplty;
  BYTE pad[68];
  BYTE lbnctt;
  BYTE title[156];
} pic_label;


void swapINT4();
void swapINT2();

/* SUSE linux seems to require int for main, not void */
int
   main(argc,argv)
int argc;
char **argv;
{

  FILE *dsk;
  int rv;
  int i,j;

  header hdr;
  BYTE *dir;
  pic_info pic;
  pic_label lb;

  unsigned int file_size, dir_size;

  unsigned int image_size;  
  BYTE *image;

  static BOOL alien=FALSE;
  static BOOL native=FALSE;
  
  printf("dskconv converts semper discs between PC and SG byte ordering\n");
  /* print out usage if no arguments supplied */
  if (argc==1){
    printf("Usage: %s <semper_disk> [alien]\n",argv[0]);
    printf("It changes  byte order to that of the machine you are using\n");
    printf("If alien is supplied, it changes to the opposite byte order\n");
    exit(1);
  }
  
  /* third argument => conversion to alien format requested */
  if(argc == 3)
    alien = TRUE;

  /* open disk file - NB original code had illegal rw+*/
  dsk = fopen(argv[1], "r+");
  if(dsk == (FILE *)NULL){
    printf("File open error\n");
    exit(1);
  }
  printf("File %s opened\n",argv[1]);

  /* read header info */
  rv = fread(&hdr, sizeof(header), 1, dsk);
  if(!rv){
    printf("Error reading header\n");
    exit(1);
  }
     
  if(strncmp(hdr.id,"Semper.disc",11)){
    printf("- not a Semper disk\n");
    exit(1);
  }
  
  /* display info */
  printf("- created by Semper version = %d.%d\n",hdr.major_version, hdr.minor_version);
  /* compute file and directory size  - surely 65536?? not 4096  */
  file_size = hdr.file_size[2]+256*hdr.file_size[1]+4096*hdr.file_size[0];
  dir_size = hdr.dir_size[0]*256+hdr.dir_size[1];

  /* allocate directory structure */
  dir = (BYTE *)malloc(dir_size*BLKSIZE);
  if(dir == (BYTE *)NULL){
    printf("Unable to allocate memory to buffer directory\n");
    exit(1);
  }
  /* read directory */
  rv = fread(dir, dir_size*BLKSIZE, 1, dsk);
  if(!rv){
    printf("Error reading directory\n");
    exit(1);
  }
  /* find first non-zero picture number */
  pic.number=0;
  j=0;
  while(!pic.number){
    memcpy(&(pic.number),&dir[j],4);
    j+=8;
  }
/* printf("found first non-zero picture number,j=%d,picnumber=%d\n",j,pic.number); */
  /* legitimate number => native file */
  if((pic.number>0)&&(pic.number<1000))
    native = TRUE;
  /* convert only if native and alien are the same */
  if(native!=alien) {
    printf("No conversion required\n");
    exit(1);
  }

  for(j=0;j<dir_size*BLKSIZE;j+=8){

    /* convert SDs and get info */
    if(!native){
      swapINT4(&dir[j]);
      swapINT4(&dir[j+4]);
      memcpy(&(pic.number),&dir[j],4);
      memcpy(&(pic.location),&dir[j+4],4);
    }
    else
      {
      memcpy(&(pic.number),&dir[j],4);
      memcpy(&(pic.location),&dir[j+4],4);
      swapINT4(&dir[j]);
      swapINT4(&dir[j+4]);
    }

    /* rewind to the directory position and write modified entry*/
#ifdef DEBUG
    printf("Trying to write SD at offset %d\n",j);
    printf("FSEEK call: BLKSIZE+j %d, SEEK_SET %x\n",BLKSIZE+j,SEEK_SET);
#endif
    if(fseek(dsk,(long)BLKSIZE+j,SEEK_SET)){
      printf("Error in seek\n");
      exit(1);
    }
    rv=fwrite(&dir[j], 8, 1, dsk);
    if(!rv){
      printf("Error writing directory entry - fwrite returned %d\n",rv);
      exit(1);
    }
    
    if(pic.number == 1000){
      printf("All pictures processed\n"); 
      exit(1);
    }

    /* report start of picture processing */
    printf("Processing image %d at block %d\n",pic.number,pic.location);

    if((pic.number > 0)&&(pic.number <1000)){
      /* read in label */
      if(fseek(dsk,(pic.location-1)*BLKSIZE,SEEK_SET)){
	printf("Error in seek\n");
	exit(1);
      }
      rv=fread(&lb, sizeof(lb), 1, dsk);
      if(!rv){
	printf("Error reading picture label\n");
	exit(1);
      }
    
      /* print out info */
      printf("%d x %d x %d (%s) Image of type %d\n",256*lb.lbnc1+lb.lbnc2,256*lb.lbnr1+lb.lbnr2,256*lb.lbnl1+lb.lbnl2,lb.title,lb.lbform);

      /* decide whether to swap */
      switch(lb.lbform){
      case IM_BYTE : printf("- byte Image - no change\n");
	break;

      case IM_INTEGER:
      case IM_FP:
      case IM_COMPLEX:

	printf("- byte-swapping\n");
	
	/* allocate memory for image */
	if(lb.lbform==IM_INTEGER)
	  image_size = ((((256*lb.lbnc1+lb.lbnc2)*sizeof(INT2)-1)/BLKSIZE)+1)*(256*lb.lbnr1+lb.lbnr2)*(256*lb.lbnl1+lb.lbnl2)*BLKSIZE;
	else
	  if(lb.lbform==IM_FP)
	    image_size = ((((256*lb.lbnc1+lb.lbnc2)*sizeof(INT4)-1)/BLKSIZE)+1)*(256*lb.lbnr1+lb.lbnr2)*(256*lb.lbnl1+lb.lbnl2)*BLKSIZE;
	  else
	    if(lb.lbform==IM_COMPLEX)
	      image_size = ((((256*lb.lbnc1+lb.lbnc2)*sizeof(INT4)*2-1)/BLKSIZE)+1)*(256*lb.lbnr1+lb.lbnr2)*(256*lb.lbnl1+lb.lbnl2)*BLKSIZE;
#ifdef DEBUG
	printf("image size = %dB\n",image_size);
#endif
	image = (BYTE *)malloc(image_size);

	/* find the image */
	if(fseek(dsk,((pic.location-1)*BLKSIZE)+256,SEEK_SET)){
	  printf("Error in seek\n");
	  exit(1);
	}

	/* read image */
	rv=fread(image, image_size, 1, dsk);
	if(!rv){
	  printf("Error reading image\n");
	  exit(1);
	}
	
	/* swap bytes */
	if(lb.lbform==IM_INTEGER)
	  for(i=0;i<image_size;i+=2)
	    swapINT2(&image[i]);
	else
	  if(lb.lbform==IM_FP)
	  for(i=0;i<image_size;i+=4)
	    swapINT4(&image[i]);
	  else
	    if(lb.lbform==IM_COMPLEX)
	      for(i=0;i<image_size;i+=8){
		swapINT4(&image[i]);
		swapINT4(&image[i+4]);
	      }
	
	/* rewind */
	if(fseek(dsk,((pic.location-1)*BLKSIZE)+256,SEEK_SET)){
	  printf("Error in seek\n");
	  exit(1);
	}
	
	/* write image */
	rv=fwrite(image, image_size, 1, dsk);
	if(!rv){
	  printf("Error writing image\n");
	  exit(1);
	}

	/* free memory */
	free(image);
	break;

      default : printf("FORMAT error\n");break;

      } /* switch */
    } /* if 0<pic<1000 */
  }
  
  /* tidy up */
  free(dir);
  fclose(dsk);
}

void
  swapINT4(buf)
BYTE *buf;
{
  BYTE tmp[4];
  
  tmp[0] = buf[3];
  tmp[1] = buf[2];
  tmp[2] = buf[1];
  tmp[3] = buf[0];
  buf[0] = tmp[0];
  buf[1] = tmp[1];
  buf[2] = tmp[2];
  buf[3] = tmp[3];
}

void
  swapINT2(buf)
BYTE *buf;
{
  BYTE tmp[2];
  
  tmp[0] = buf[1];
  tmp[1] = buf[0];
  buf[0] = tmp[0];
  buf[1] = tmp[1];
}
