int fac(int a){
    if(a==1) return 1;
    return a*fac(a-1);
}
int main(){
    int x;
    x=read();
    write(fac(x));
    return 1;
}