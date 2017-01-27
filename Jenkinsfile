node {
    env.PATH = "~/.cargo/bin:${env.PATH}"
    checkout scm
    sh "./autogen.sh"
    sh "./configure"
    sh "make -j 2"
    sh "make check"
}
