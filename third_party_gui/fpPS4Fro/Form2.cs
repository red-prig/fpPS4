using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Ini;

namespace fpPS4Fro
{
    public partial class Form2 : Form
    {
        string inipath = System.IO.Directory.GetCurrentDirectory();
        public Form2()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                textBox1.Text = folderBrowserDialog1.SelectedPath;
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                textBox2.Text = folderBrowserDialog1.SelectedPath;
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            string emu = "";
            string games = "";
            string sd = "";

            emu = textBox1.Text.ToString();
            games = textBox2.Text.ToString();
            sd = textBox3.Text.ToString();
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            ini.IniWriteValue("DIR","emu",emu );
            ini.IniWriteValue("DIR","games", games);
            ini.IniWriteValue("DIR", "savedata", sd);
            Close();
        }

        private void button4_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void Form2_Load(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            textBox1.Text = ini.IniReadValue("DIR", "emu");
            textBox2.Text = ini.IniReadValue("DIR", "games");
            textBox3.Text = ini.IniReadValue("DIR", "savedata");
        }

        private void button5_Click(object sender, EventArgs e)
        {
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                textBox3.Text = folderBrowserDialog1.SelectedPath;
            }
        }
    }
}
