#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

typedef pair<double,double> pdd;

/*
double weight[]={3.312544545960886, -4.271673738238844, -0.0306744503696473, 
-0.035737775555589206, -9.05997837557413, 8.065721999298216, 
-9.951255048272829, 0.5191494865538622,-0.11778896565517548,
-6.744410320848491, -0.044935853653590374, 0.08404797911146185,
-0.4937934609174337, -9.946750633760296, 9.608859394428517,
8.641842039511499, 1.8161711755917087,0.08593627638304265, 
-0.21219634541402566,-0.1012376433644313, -9.648208553801776};
*/

double weight[]={3.2146072750756245,-4.547408641981711,-0.02740931868295622, 
-0.032245039764752986,-9.22502555042529,8.177966693024068, 
-10.669981357432707,0.5391224636755709,-0.0823284144425754,
-5.772816776784273,-0.04491596780273607,0.041278708895674344, 
-0.3984762828070597,-11.712473199659712,11.048139257927923, 
8.89133958772934,3.5070065256660863,0.08549145010429933, 
-0.20873212217797565,-0.026263335977116803,-11.171123084091654, 
-0.00013383269570865265,4.05162304496626,9.986309361488505,
51,5.432732732920991};

const int INF=1000100010;
int NORM;

class ImageFromBlocks {
public:
    int H,W,BS,IH,IW,remGRID;
	int mxDiscard,remDiscard;
	int pH,pW;
	double curscore=0;
	int grid[101][101];
	vector<int> IMG;
	int remH[101];
	int lowb[4],nowh[101];
    double imgstd=0;

	int turn=0;
    bool drop[100101];

	pdd getEvalscore(int x,int y,vector<int>& piece){
		double score=0,ept=0,hstd=0,bmp=0;
		int cnt=0;
		for(int i=pH-1;i>=0;--i)rep(j,pW){
			if(piece[i*pW+j]!=-1){
				int ty=(y+i)*BS,tx=(x+j)*BS;
				int col=piece[i*pW+j],diff=0;
				FOR(cy,ty,ty+BS)FOR(cx,tx,tx+BS){
					diff+=abs(((col>>16)&0xff)-((IMG[cy*IW+cx]>>16)&0xff));
					diff+=abs(((col>>8)&0xff)-((IMG[cy*IW+cx]>>8)&0xff));
					diff+=abs(((col)&0xff)-((IMG[cy*IW+cx])&0xff));
				}
				score+=(1.0-(double)diff/NORM)*(1.0-(double)diff/NORM);
				nowh[x+j]=y+i;
				++cnt;
			}
			else if(lowb[j]<=i&&grid[y+i][x+j]==-1){
				ept+=1;
			}
			if(i==pH-1){
				ept+=max(0,remH[x+j]-y-pH);
			}
		}

		if(x-2>=0) nowh[x-2]=remH[x-2];
        if(x-1>=0) nowh[x-1]=remH[x-1];
        if(x+pW<W) nowh[x+pW]=remH[x+pW];
        if(x+pW+1<W) nowh[x+pW+1]=remH[x+pW+1];
        int hole=0;
        FOR(i,x-1,x+pW+1){
            if(i-1>=0&&i+1<W){
                if(abs(nowh[i-1]-nowh[i])>=3&&abs(nowh[i+1]-nowh[i])>=3) hole+=abs(nowh[i-1]-nowh[i])+abs(nowh[i+1]-nowh[i])-4;
            }
            else if(i-1>=0){
                if(abs(nowh[i-1]-nowh[i])>=3) hole+=abs(nowh[i-1]-nowh[i])*2-4;
            }
            else if(i+1<W){
                if(abs(nowh[i+1]-nowh[i])>=3) hole+=abs(nowh[i+1]-nowh[i])*2-4;
            }
        }
        double dropratio=0;
        if(turn>=2){
            rep(i,min(turn,10)){
                if(drop[turn-i-1]) dropratio+=1.0;
            }
        }
        dropratio/=10;


		double sum=0,sqsum=0;
		rep(j,W){
			if(j>=x&&j<x+pW){
				sum+=nowh[j];
				sqsum+=nowh[j]*nowh[j];
				if(j!=0&&j==x){
					bmp+=abs(nowh[j]-remH[j-1]);
				}
				else if(j!=0){
					bmp+=abs(nowh[j]-nowh[j-1]);
				}
			}
			else{
				sum+=remH[j];
				sqsum+=remH[j]*remH[j];
				if(j==x+pW){
					bmp+=abs(remH[j]-nowh[j-1]);
				}
			}
		}
		hstd=sqrt(sqsum/W-(sum/W)*(sum/W));
		bmp*=bmp;
		double rt1=(1.0-(double)remGRID/(H*W));
        double rt2=(1.0-(double)remDiscard/mxDiscard);
        double bias=remDiscard==0?10000:0;

        double eval=0;
        eval=
            score/cnt*4*weight[0]
            +ept*weight[1]
            +hstd*weight[2]
            +bmp*weight[3]
            +rt1*weight[4]
            +rt2*weight[5]
            +(rt1+weight[14])/(rt2+weight[14])*weight[6]
            +hstd*W/100*weight[7]
            +bmp*W/100*weight[8]
            +rt1*rt2*weight[9]
            +bmp*rt1*weight[10]
            +bmp*rt2*weight[11]
            +weight[12]
            +weight[12]*H*W/10000*weight[13]
			+rt1*rt1*weight[15]
            +rt2*rt2*weight[16]
            +imgstd*weight[17]
			+imgstd*H*W/10000*weight[18]
			+(double)H*W/mxDiscard*weight[19]
			+(rt1-rt2)*weight[20]
            +bias

			//+hole*weight[21]
            //+dropratio*weight[22]
			+hole*weight[21]
            +dropratio*dropratio*dropratio*(ept==0?weight[22]:weight[23])
			+(H*W>100*weight[24]?+(1.0-rt1)*(1.0-rt1)*weight[25]:0)
            ;
		return {eval,score};
	}
	
	void rotated(vector<int>& piece){
		vector<int> res(pW*pH);
		rep(i,pH)rep(j,pW){
			res[j*pH+pH-i-1]=piece[i*pW+j];
		}
		swap(pH,pW);
		piece=res;
	}
	string init(int imageHeight, int blockSize, int maxDiscard, vector<int> image) {
		IH=imageHeight,IW=image.size()/IH;
		H=imageHeight/blockSize;
	    W=image.size()/imageHeight/blockSize;
		BS=blockSize;
		NORM=BS*BS*765;
		remGRID=H*W;

		mxDiscard=maxDiscard;
		remDiscard=maxDiscard;

		IMG=image;
		memset(grid,-1,sizeof(grid));
		rep(i,W) remH[i]=H;

        double sqsum[3]={},sum[3]={};
		int iSZ=image.size();

		rep(i,iSZ){
			int col=(image[i]>>16)&0xff;
			sum[0]+=col,sqsum[0]+=col*col;
			col=(image[i]>>8)&0xff;
			sum[1]+=col,sqsum[1]+=col*col;
			col=(image[i])&0xff;
			sum[2]+=col,sqsum[2]+=col*col;
		}
		rep(i,3){
			imgstd+=sqrt(sqsum[i]/iSZ-sum[i]/iSZ*sum[i]/iSZ);
		}
		imgstd/=3;

		cerr<<H<<" "<<W<<endl;
        //cerr<<imgstd<<endl;

	    return "";
	}

	string placePiece(int pieceHeight, vector<int> piece, int timeLeft) {
	    pH=pieceHeight;
		pW=piece.size()/pieceHeight;
		string ret;
		//double mx=-1e9,nxscore=0;
		double mx=0,nxscore=0;
		pint ans={-1,-1};
		int nxy=-1;
		rep(rot,4){
			rep(j,pW){
				for(int i=pH-1;i>=0;--i)if(piece[i*pW+j]!=-1){
					lowb[j]=i+1;break;
				}
			}
			rep(xpos,W)if(xpos+pW<=W){
				int ypos=INF;
				rep(j,pW){
					ypos=min(ypos,remH[xpos+j]-lowb[j]);
				}
				if(ypos<0) continue;
				pdd tscore=getEvalscore(xpos,ypos,piece);
				if(tscore.first>=mx){
					mx=tscore.first;
					nxscore=tscore.second;
					ans={rot,xpos};
					nxy=ypos;
				}
			}

			rotated(piece);
		}
		if(ans.first==-1){
			if(remDiscard>0){
				--remDiscard;
				ret="D";
			}
			else ret=to_string(0)+" "+to_string(0);;
		}
		else{
			int nxr=ans.first,nxx=ans.second;
			ret=to_string(ans.first)+" "+to_string(ans.second);
			rep(i,nxr) rotated(piece);
			for(int i=pH-1;i>=0;--i)rep(j,pW)if(piece[i*pW+j]!=-1){
				grid[nxy+i][nxx+j]=piece[i*pW+j];
				//remGRID-=remH[nxx+j]-nxy+i;
				remH[nxx+j]=nxy+i;
			}
			remGRID=0;
			rep(i,W){
				remGRID+=remH[i];
			}
			curscore+=nxscore;
		}
		//cerr<<curscore<<" "<<mx<<" "<<ret<<endl;
		if(ret=="D")drop[turn]=true;
        ++turn;
	    return ret;
	}
};
// -------8<------- end of the solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < (int)v.size(); ++i)
        cin >> v[i];
}

int main() {
    ImageFromBlocks sol;
    int imageHeight;
    cin >> imageHeight;
    int blockSize;
    cin >> blockSize;
    int maxDiscard;
    cin >> maxDiscard;
    int imageLength;
    cin >> imageLength;
    vector<int> image(imageLength);
    getVector(image);
    
    string ret = sol.init(imageHeight, blockSize, maxDiscard, image);
    cout << ret << endl;
    cout.flush();

	while (true) {
	    int pieceHeight;
	    cin >> pieceHeight;
	    int pieceLength;
	    cin >> pieceLength;
	    vector<int> piece(pieceLength);
	    getVector(piece);
	    int timeLeft;
	    cin >> timeLeft;
		ret = sol.placePiece(pieceHeight, piece, timeLeft);
	    cout << ret << endl;
	    cout.flush();
	}
}

