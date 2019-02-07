#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<ll,ll> pint;

double timeLimit=9.97;
 
const int64_t CYCLES_PER_SEC=2800000000;
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	void plus(double a) { start -= (a * CYCLES_PER_SEC); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};
unsigned long w;
unsigned long xor128(){
  static unsigned long x=123456789,y=362436069,z=521288629;
  unsigned long t=(x^(x<<11));
  x=y;y=z;z=w;
  return (w=(w^(w >> 19))^(t^(t>>8)));
}
class SameColorPairs {
public:
    int H,W;
    char used;
    vector<string> ret,ans,tmp,ini1,ini2,tr1,tr2;
    bool update;
    ll pp,pp2,base;
    int dp[101];
    void param_init(vector<string> &board){
        pp2=pp;ret.clear();//board=tmp;
    }
    void add(int y1,int x1,int y2,int x2,vector<string> &board){
        ret.push_back(to_string(y1)+" "+to_string(x1)+" "+to_string(y2)+" "+to_string(x2));
        board[y1][x1]=used;board[y2][x2]=used;
        update=true;
    }
    void init_remove(vector<string> &board){
        rep(i,H){
            char cur=used;
            int pos=0;
            rep(j,W){
                if(cur==board[i][j]) continue;
                else{
                    if(cur!=used&&j-pos>=2){
                        if((j-pos)%2==1) add(i,pos,i,pos+2,board),pos+=3;
                        else add(i,pos,i,pos+1,board),pos+=2;
                        FOR(k,pos,j){
                            add(i,k,i,k+1,board),++k;
                        }
                    }
                    cur=board[i][j],pos=j;
                }
            }
            if(cur!=used&&W-pos>=2){
                if((W-pos)%2==1) add(i,pos,i,pos+2,board),pos+=3;
                else add(i,pos,i,pos+1,board),pos+=2;
                FOR(k,pos,W){
                    add(i,k,i,k+1,board),++k;
                }
            }
        }
        rep(j,W){
            char cur=used;
            int pos=0;
            rep(i,H){
                if(cur==board[i][j]) continue;
                else{
                    if(cur!=used&&i-pos>=2){
                        if((i-pos)%2==1) add(pos,j,pos+2,j,board),pos+=3;
                        else add(pos,j,pos+1,j,board),pos+=2;
                        FOR(k,pos,i){
                            add(k,j,k+1,j,board),++k;
                        }
                    }
                    cur=board[i][j],pos=i;
                }
            }
            if(cur!=used&&H-pos>=2){
                if((H-pos)%2==1) add(pos,j,pos+2,j,board),pos+=3;
                else add(pos,j,pos+1,j,board),pos+=2;
                FOR(k,pos,H){
                    add(k,j,k+1,j,board),++k;
                }
            }
        }
    }
    void init_remove2(vector<string> &board){
        rep(j,W){
            char cur=used;
            int pos=0;
            rep(i,H){
                if(cur==board[i][j]) continue;
                else{
                    if(cur!=used&&i-pos>=2){
                        if((i-pos)%2==1) add(pos,j,pos+2,j,board),pos+=3;
                        else add(pos,j,pos+1,j,board),pos+=2;
                        FOR(k,pos,i){
                            add(k,j,k+1,j,board),++k;
                        }
                    }
                    cur=board[i][j],pos=i;
                }
            }
            if(cur!=used&&H-pos>=2){
                if((H-pos)%2==1) add(pos,j,pos+2,j,board),pos+=3;
                else add(pos,j,pos+1,j,board),pos+=2;
                FOR(k,pos,H){
                    add(k,j,k+1,j,board),++k;
                }
            }
        }
        rep(i,H){
            char cur=used;
            int pos=0;
            rep(j,W){
                if(cur==board[i][j]) continue;
                else{
                    if(cur!=used&&j-pos>=2){
                        if((j-pos)%2==1) add(i,pos,i,pos+2,board),pos+=3;
                        else add(i,pos,i,pos+1,board),pos+=2;
                        FOR(k,pos,j){
                            add(i,k,i,k+1,board),++k;
                        }
                    }
                    cur=board[i][j],pos=j;
                }
            }
            if(cur!=used&&W-pos>=2){
                if((W-pos)%2==1) add(i,pos,i,pos+2,board),pos+=3;
                else add(i,pos,i,pos+1,board),pos+=2;
                FOR(k,pos,W){
                    add(i,k,i,k+1,board),++k;
                }
            }
        }
    }
    void remove_cross(vector<string> &board){
        rep(i,H-1)rep(j,W-1){
            if(board[i][j]!=used&&board[i][j]==board[i+1][j+1]&&board[i+1][j]==used&&board[i][j+1]==used){
                add(i,j,i+1,j+1,board);
            }
            if(board[i][j+1]!=used&&board[i][j+1]==board[i+1][j]&&board[i][j]==used&&board[i+1][j+1]==used){
                add(i,j+1,i+1,j,board);
            }
        }
    }
    void remove_horizon(vector<string> &board){
        rep(i,H){
            int pos=-1;
            char co=used;
            rep(j,W){
                if(board[i][j]==used) continue;
                else if(co!=used&&co==board[i][j]){
                    add(i,pos,i,j,board),co=used;
                }
                else if(co==used){
                    co=board[i][j];pos=j;
                }
                else if(co!=used&&co!=board[i][j]){
                    co=board[i][j];pos=j;
                }
            }
        }
    }
    void remove_vertical(char co,int y,int x,vector<string> &board){
        int h=H;
        FOR(j,x,W){
            int l=max(y,dp[j]);
            if(h-y<=1) break;
            FOR(i,l,h){
                if(j==x&&i==y) continue;
                if(board[i][j]!=used&&board[i][j]!=co){
                    h=i;break;
                }
                if(board[i][j]==co){
                    add(y,x,i,j,board);dp[j]=i;
                    return;
                }
            }
        }
        h=H;
        for(int j=x;j>=0;--j){
            int l=max(y,dp[j]);
            if(h-y<=1) break;
            FOR(i,l,h){
                if(j==x&&i==y) continue;
                if(board[i][j]!=used&&board[i][j]!=co){
                    h=i;break;
                }
                if(board[i][j]==co){
                    add(y,x,i,j,board);dp[j]=i;
                    return;
                }
            }
        }
    }
    void remove_vertical2(char co,int y,int x,vector<string> &board){
        int h=-1;
        for(int j=x;j>=0;--j){
            int l=min(y,dp[j]);
            if(y-h<=1) break;
            for(int i=l;i>h;--i){
                if(j==x&&i==y) continue;
                if(board[i][j]!=used&&board[i][j]!=co){
                    h=i;break;
                }
                if(board[i][j]==co){
                    add(y,x,i,j,board);dp[j]=i;
                    return;
                }
            }
        }
        h=-1;
        FOR(j,x,W){
            int l=min(y,dp[j]);
            if(y-h<=1) break;
            for(int i=l;i>h;--i){
                if(j==x&&i==y) continue;
                if(board[i][j]!=used&&board[i][j]!=co){
                    h=i;break;
                }
                if(board[i][j]==co){
                    add(y,x,i,j,board);dp[j]=i;
                    return;
                }
            }
        }
    }
    vector<string> removePairs(vector<string> board) {
        Timer timer;
        timer.reset();
        H=board.size(),W=board[0].size(),used='9';base=70;pp=85;w=88675123;
        vector<int> que,target;
        int sz,cur=0;
        bool flag=false;
        int rp=1000;
        tmp=board,ini1=board,ini2=board;
        init_remove(ini1);tr1=ret;ret.clear();
        init_remove2(ini2);tr2=ret;
        ans=tr1.size()>tr2.size()?tr1:tr2;
        double sub,stime;
        rep(k,2000){
            if(k==0) stime=timer.get();
        if(!flag||flag&&target[cur]==0){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini1;ret=tr1;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(0);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==1){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini1;ret=tr1;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(1);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==2){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini1;ret=tr1;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(2);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==3){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini1;ret=tr1;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(3);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==4){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini2;ret=tr2;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(4);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==5){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini2;ret=tr2;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(5);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==6){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini2;ret=tr2;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(6);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(!flag||flag&&target[cur]==7){
            cur=(cur+1)%sz;
            param_init(board);
            board=ini2;ret=tr2;
            rep(q,rp){
                update=false;
                remove_cross(board);remove_horizon(board);
                if(xor128()%100<50){
                    memset(dp,0,sizeof(dp));
                    rep(i,H-1)for(int j=W-1;j>=0;--j)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical(board[i][j],i,j,board);
                    }
                }
                else{
                    rep(i,W) dp[i]=101;
                    for(int i=H-1;i>=1;--i)rep(j,W)if(board[i][j]!=used&&xor128()%100<=pp2){
                        remove_vertical2(board[i][j],i,j,board);
                    }
                }
                if(!update&&pp2==100) break;
                else if(!update) pp2=100;
            }
            if(!flag&&ans.size()<=ret.size()) ans=ret,que.pb(7);
            else if(ans.size()<ret.size()) ans=ret;
        }
        if(k==0){
            sub=timer.get()-stime;
            timeLimit-=sub/1.5;
        }
        double now=timer.get();
        if(!flag&&now>8.5){
            if(que.empty()) return ans;
            flag=true;
            while(!que.empty()&&target.size()<3){
                target.pb(que.back()),que.pop_back();
                sort(target.begin(),target.end());
                target.erase(unique(target.begin(),target.end()),target.end());
            }
            sz=target.size();
            que.clear();    
            cur=0;
            pp=85;         
        }
        if(now>timeLimit){
            break;
        }
        cur=0;
        }
        return ans;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    SameColorPairs scp;
    int H;
    cin >> H;
    vector<string> board(H);
    getVector(board);

    vector<string> ret = scp.removePairs(board);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
