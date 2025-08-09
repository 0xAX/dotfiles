import subprocess

def get_pass():
    passwd = subprocess.getoutput('op item get qvmlw4o5342mdtofsdpxaorvdi --reveal | grep password | sed -e "s/  password:    //"')
    return passwd
