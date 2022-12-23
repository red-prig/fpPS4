using Ini;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection.Metadata.Ecma335;
using System.Text;

namespace fpPS4Fro
{
    public partial class Form1 : Form
    {
        string emp = "";
        string gap = "";
        string sadi = "";
        string inipath = System.IO.Directory.GetCurrentDirectory();
        delegate void VoidDelegate();
        public Form1()
        {
            InitializeComponent();
        }

        private void dateiToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void toolStrip1_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {

        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            string befehl = "";
            string arg = "";
            int m = 0;

            if (checkBox7.Checked == true)
                m = 2;
            else
                m = 1;

            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                string path = openFileDialog1.FileName;
                befehl = @"e:\emu\fpps4\fpps4.exe";
                arg = " -e "+ "\""+ path + "\"";
                rungame(befehl, arg, m);
            }
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            string emu = "";
            string games = "";

            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            emu = ini.IniReadValue("DIR", "emu");
            games = ini.IniReadValue("DIR", "games");
            sadi = ini.IniReadValue("DIR", "savedata");

            if (emu.Length < 1)
                MessageBox.Show("Please set the directories under OPTIONS");

            if (ini.IniReadValue("HACKS", "H1") == "1")
                checkBox1.Checked = true;
            else
                checkBox1.Checked = false;
            if (ini.IniReadValue("HACKS", "H2") == "1")
                checkBox2.Checked = true;
            else
                checkBox2.Checked = false;

            if (ini.IniReadValue("HACKS", "H3") == "1")
                checkBox3.Checked = true;
            else
                checkBox3.Checked = false;

            if (ini.IniReadValue("HACKS", "H4") == "1")
                checkBox4.Checked = true;
            else
                checkBox4.Checked = false;

            if (ini.IniReadValue("HACKS", "H5") == "1")
                checkBox5.Checked = true;
            else
                checkBox5.Checked = false;

            if (ini.IniReadValue("HACKS", "H6") == "1")
                checkBox6.Checked = true;
            else
                checkBox6.Checked = false;

            if (ini.IniReadValue("ETC", "W1") == "1")
            {
                checkBox7.Checked = true;
                panel2.Visible = true;
                button1.Visible= true;

            }
            else
                checkBox7.Checked = false;

            if (ini.IniReadValue("ETC", "SD") == "1")
            {
                checkBox8.Checked = true;
            }
            else
                checkBox8.Checked = false;


            emp = emu;
            gap = games;

            if(games.Length> 0)
               spiele(games);
        }

        void spiele (string pfad)
        {
            string pfad1="";
            int i = 0;

            listView1.Items.Clear();

            DirectoryInfo ParentDirectory = new System.IO.DirectoryInfo(pfad);

            foreach (DirectoryInfo d in ParentDirectory.GetDirectories())
            {
                ListViewItem directories = new ListViewItem();
                directories.Text = d.Name;
                directories.SubItems.Add("Games");
                pfad1 = pfad + @"\" + directories.Text + @"\pict.jpg";
                if(File.Exists(pfad1))
                {
                    imageList1.Images.Add(Image.FromFile(pfad1));
                    directories.ImageIndex= i;
                    i ++;
                }
                listView1.Items.Add(directories);
               
            }
        }

        private void toolStripButton2_Click(object sender, EventArgs e)
        {
            Form2 frm = new Form2();
            frm.ShowDialog();
            if (frm.DialogResult == DialogResult.OK)
            {
                IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
                emp = ini.IniReadValue("DIR", "emu");
                gap = ini.IniReadValue("DIR", "games");
                sadi = ini.IniReadValue("DIR", "savedata");
                spiele(gap);
            }
        }

  
        
        private void loadGameToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string befehl = "";
            string arg = "";
            string emu = "";
            int m = 0;

            if (checkBox7.Checked == true)
                m = 2;
            else
                m = 1;

 
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            befehl = ini.IniReadValue("DIR", "emu");
            emu = befehl +@"\";
            befehl = befehl + @"\fpps4.exe";

            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                string path = openFileDialog1.FileName;
                arg = " -e " + "\"" + path + "\"";
                arg = argm(arg);
                rungame(befehl, arg, m);
 
            }
        }

        string argm(string arg)
        {
            return arg;
        }

        private void toolStripButton4_Click(object sender, EventArgs e)
        {
            AboutBox1 frm = new AboutBox1();
            frm.ShowDialog();
        }

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox1.Checked == true )
            {
                ini.IniWriteValue("HACKS", "H1", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H1", "0");
            }
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox2.Checked == true)
            {
                ini.IniWriteValue("HACKS", "H2", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H2", "0");
            }
        }

        private void checkBox3_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox3.Checked == true)
            {
                ini.IniWriteValue("HACKS", "H3", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H3", "0");
            }
        }

        private void checkBox4_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox4.Checked == true)
            {
                ini.IniWriteValue("HACKS", "H4", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H4", "0");
            }
        }

        private void checkBox5_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox5.Checked == true)
            {
                ini.IniWriteValue("HACKS", "H5", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H5", "0");
            }
        }

        private void checkBox6_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox6.Checked == true)
            {
                ini.IniWriteValue("HACKS", "H6", "1");
            }
            else
            {
                ini.IniWriteValue("HACKS", "H6", "0");
            }
        }

 
        private void checkBox7_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox7.Checked == true)
            {
                ini.IniWriteValue("ETC", "W1", "1");
                panel2.Visible = true;
                button1.Visible = true;
            }
            else
            {
                ini.IniWriteValue("ETC", "W1", "0");
                panel2.Visible = false;
                button1.Visible = false;
            }
        }

        private void listView1_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        private void listView1_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            string befehl = "";
            string arg = "";
            string path = "";
            int m = 0;

            if (this.listView1.SelectedItems.Count == 0)
                return;

            string namn = this.listView1.SelectedItems[0].Text;

            befehl = emp + @"\fpps4.exe";
            path = gap + @"\" + namn + @"\eboot.bin";
            arg = " -e " + "\"" + path + "\"";

            if (checkBox7.Checked == true)
                m = 2;
            else
                m = 1;

            rungame(befehl, arg, m);
 
        }

        void rungame(string command,string arg,int m)
        {
  
            StringBuilder output = new StringBuilder();
            int lineCount = 0;


            if (checkBox8.Checked == true)
                arg = arg + " -s " + sadi;

            if (checkBox1.Checked == true)
                arg = arg + " -h " + "DEPTH_DISABLE_HACK";
            if (checkBox2.Checked == true)
                arg = arg + " -h " + "COMPUTE_DISABLE_HACK";
            if (checkBox3.Checked == true)
                arg = arg + " -h " + "MEMORY_BOUND_HACK";
            if (checkBox4.Checked == true)
                arg = arg + " -h " + "IMAGE_TEST_HACK";
            if (checkBox5.Checked == true)
                arg = arg + " -h " + "IMAGE_LOAD_HACK";
            if (checkBox6.Checked == true)
                arg = arg + " -h " + "DISABLE_SRGB_HACK";


            Directory.SetCurrentDirectory(emp + @"\");

            toolStripStatusLabel1.Text = command + " " + arg;
            textBox1.Text = command + " " + arg;

            if (m == 1)
            {
                Process P = new Process();
                P.StartInfo.FileName = command;
                P.StartInfo.Arguments = arg;
                P.Start();
            }
            
            if (m == 2)
            {
                richTextBox1.Text = "";
                Proc(command, arg);
            } 
        }

        private void Proc(string command, string arg)
        {
            BackgroundWorker bgw = new BackgroundWorker();
            bgw.DoWork += (sender, args) => {
                Process process = new Process();
                process.StartInfo.FileName = command;
                process.StartInfo.Arguments = arg;
                process.StartInfo.UseShellExecute = false;
                process.StartInfo.RedirectStandardOutput = true;
                process.StartInfo.CreateNoWindow = true;
                process.Start();
                if (process != null)
                {
                    process.OutputDataReceived += ((s, ev) =>
                    {
                        string sData = ev.Data;
                        sData += "\r\n";
                        ControlInvoke(richTextBox1, () => richTextBox1.Text += sData);
                    });
                    process.BeginOutputReadLine();
                }
            };
            bgw.RunWorkerAsync();
        }
        public static void ControlInvoke(Control control, Action function)
        {
            if (control.IsDisposed || control.Disposing)
                return;
            if (control.InvokeRequired)
            {
                control.Invoke(new VoidDelegate(() => ControlInvoke(control, function)));
                return;
            }
            function();
        }


        private void checkBox8_CheckedChanged(object sender, EventArgs e)
        {
            IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
            if (checkBox8.Checked == true)
            {
                ini.IniWriteValue("ETC", "SD", "1");
            }
            else
            {
                ini.IniWriteValue("ETC", "SD", "0");
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Clipboard.SetText(richTextBox1.Text.ToString());
        }

        private void optionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Form2 frm = new Form2();
            frm.ShowDialog();
            if (frm.DialogResult == DialogResult.OK)
            {
                IniFile ini = new IniFile(inipath + @"\fpps4fro.ini");
                emp = ini.IniReadValue("DIR", "emu");
                gap = ini.IniReadValue("DIR", "games");
                sadi = ini.IniReadValue("DIR", "savedata");
            }
        }
    }
}