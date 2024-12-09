yum install -y epel-release deltarpm
yum -y groupinstall "Development Tools"
yum install -y git python-pip libvirt-devel python-devel
git clone https://github.com/sjjf/md_server.git
cd md_server
pip install -r requirements.txt
python setup.py bdist_rpm
cp requirements.txt dist/* /export
ls -l /export
